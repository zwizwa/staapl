#lang racket/base

;; Generic tools for implementing Forth syntax for macro/target based
;; languages as rpn parser extensions (struct rpn-transformer).

(require "../rpn.rkt"
         ;; "../tools.rkt"
         "../tools/io.rkt"
         "../tools/stx.rkt"
         )

(provide (all-defined-out))


;; PATH for toplevel

;; During the execution of the 'rpn-begin expander, the struct
;; contained in the 'current-forth-path parameter is updated to
;; reflect the current filesystem context as we move through the
;; tokens.  This context includes the file currently expanding and the
;; filesystem search path for relative filenames.  Whenever the
;; 'rpn-begin expansion is exited to allow top-level forms to be
;; expanded, this state is dumped in a syntax object so it can be
;; restored on 'rpn-begin re-entry.


(define-struct forth-path (here search) #:mutable)
(define current-forth-path (make-parameter (make-forth-path "." '())))


(define (forth-path-add! p)
  (let ((fp (current-forth-path)))
    (set-forth-path-search! fp (cons p (forth-path-search fp)))))

(define (forth-here! p)
  (set-forth-path-here! (current-forth-path) p))
(define (forth-here)
  (forth-path-here (current-forth-path)))
(define (forth-search)
  (forth-path-search (current-forth-path)))

;; Dump the current path as a syntax transformer that resets the
;; parameter when invoked.
(define (forth-path-dump)
  (let ((fp (current-forth-path)))
    ;; (printf "forth-path-dump:\n~s\n~s\n" (forth-path-here fp) (forth-path-search fp))
    (rpn-begin! (current-forth-path fp))))

(define (make-rpn-path-transformer stx->path)
  (make-rpn-transformer
   (lambda (w d k)
     (forth-path-add! (stx->path (w-cadr w)))
     (k (w-cddr w) d))))

;; Side effect
(define-syntax-rule (rpn-begin! . body)
  (make-rpn-transformer
   (lambda (w d k)
     (begin . body)
     (k (w-cdr w) d))))




;; PARSER EXTENSIONS


;; Prefix syntax for local variables.
(define (make-rpn-locals-transformer locals)
  (make-rpn-transformer
   (lambda (w d k)
     (let next ((w (w-cdr w))
                (l '()))
       (let ((f (w-car w)))
         (if (eq? '\| (syntax->datum f))
             (k (w-cdr w)
                (d-compile #`(#,locals #,(reverse l)) d))
             (next (w-cdr w)
                   (cons f l))))))))

;; Prefix syntax for definition start.
(define (make-rpn-definition-transformer compile)
  (make-rpn-same-definition-transformer (lambda (d) compile)))

;; Same, but use semantics of last entry.  This uses a convenience
;; macro that reads from the input stream and binds pattern names.
(define (make-rpn-same-definition-transformer get-compile)
  (rpn-dict-rule d (name)
                 (let ((compile (get-compile d))) ;; allow inspection of dict
                   (let* ((d (d-start d))
                          (d (compile #'name d))) d))))


;; Straight compile of a toplevel expression.  This will insert the
;; expression before the one currently being compiled.
(define (rpn-compile-toplevel stx d)
  (d-insert (syntax->list/recertify stx) d))

;; Prefix syntax for scheme s-expressions.
(define (make-rpn-sexp-transformer open close compile-sexp)
  (make-rpn-transformer
   (lambda (w-in d k)
     (define (collect w-start)
       (let next ((w  w-start)
                  (l '()))
         ;; (printf "W: ~a\n" (let ((x (syntax->datum (car w)))) (list x (eq? open x) (eq? close x))))
         (when (w-null? w)
           (raise-syntax-error
            #f (format "expected '~a', sexp starts" close)
            (car w-in)))
         (let ((w-sym (syntax->datum (w-car w)))
               (w+ (w-cdr w)))
           (cond
            ((eq? open w-sym)
             (let-values (((w-next lst) (collect w+)))
               (next w-next
                     (cons lst l))))
            ((eq? close w-sym)
             (values w+
                     (datum->syntax #f (reverse l)))) ;; pack in stx object
            (else
             (next w+
                   (cons (w-car w) l)))))))
     (let-values (((w-next s) (collect (w-cdr w-in))))
       (k w-next (compile-sexp s d))))))

;; Parameters and dynamic parser state.

;; The Forth parser needs to maintain state while parsing a file.
;; This includes current toplevel forms to be installed and the
;; current "mode".  This transformer installs such a dynamic state for
;; the remainder of the parser input.

(define (make-rpn-parameterize-transformer param init-state compile-state)
  (lambda (w d k)
    (let-values
        (((d+ k)
          (parameterize ((param init-state))
            (printf "nesting: ~a\n" param)
            (rpn-parse-nested (w-cdr w) d k))))
      (k '() (compile-state (param) d+)))))



;; Prefix syntax for including files.  This uses the
;; 'current-forth-path parameter and mutates the forth-path struct it
;; contains.
(define (make-rpn-include-transformer file->syntax
                                      filename-syntax->string
                                      [logger void])
  (make-rpn-transformer
   (lambda (w d k)
     (let ((here (forth-here))) ;; save current location
       ;; (printf "path: ~a\n" (current-search-path))
       (let ((filename-stx (w-cadr w))
             (w-rest       (w-cddr w)))
       (let* ([filename
               (filename-syntax->string filename-stx)]
              [search-path
               (cons here (forth-search))]
              [resolved-filename
               (resolve-path-list filename search-path)]
              [dir
               (filename->path resolved-filename)])
         (logger resolved-filename)
         (forth-here! dir)
         (k (w-append
             (file->syntax resolved-filename filename-stx)
             (list (rpn-begin! (forth-here! here)))
             w-rest)
            d)))))))




;; When dictionary forms expand to forms that might introduce syntax
;; bindings, recursive expansion is necessary to postpone further
;; parsing until the defining forms are processed.
(define (make-rpn-expand-transformer begin-stx-thunk)
  (make-rpn-transformer
   (lambda (w d k)
     (let* ((header (begin-stx-thunk))  ;; might include dynamic context (??)
            (footer (w-cdr w))
            (form   #`(#,@header #,@footer))
            (form/r (syntax-recertify form header (current-code-inspector) #f)))
     (k '() (rpn-compile-toplevel form/r d))))))







;; Convert a syntax expression for a header into a compiler used in
;; the forms above.

(define (rpn-make-header->compile make-header)
  (lambda (n d)
    (foldl d-compile d (syntax->list/recertify (make-header n)))))


(define (make-rpn-forth-definition-transformer make-header)
  (make-rpn-definition-transformer
   (rpn-make-header->compile make-header)))

(define rpn-curly-brace-transformer
   (make-rpn-sexp-transformer
    (string->symbol "{")
    (string->symbol "}")
    rpn-compile-toplevel))

;; TOPLEVEL

(define (make-rpn-quotation-transformer compile)
  (let ((open (string->symbol "["))
        (close (string->symbol "]")))
    (make-rpn-sexp-transformer
     open close
     (lambda (expr dict)
       (d-compile (compile expr) dict)))))


;; Pass the rest of the input tokens to a prefix parser as a single
;; list form.
(define rpn-slurp
  (make-rpn-transformer
   (lambda (w d k)
     (let ((parser (w-cadr w))
           (rest   (w-cddr w)))
       (k (list parser rest) d)))))


;; TODO (from parser-tx.ss)

;; TOPLEVEL EXPRESSIONS
              
;; (define (require-tx code expr)
;;   (define (p x) (symbol->string (syntax->datum x)))
;;   (define (next e c)
;;     (register-toplevel e)
;;     ((rpn-next) c expr))

;;   (syntax-case code (planet staapl)
;;     ((_ planet module . code+) (next `(require (planet ,(p #'module))) #'code+))
;;     ((_ staapl module . code+) (next `(require (planet ,(p #'module) ("zwizwa" "staapl.plt"))) #'code+))
;;     ((_ module . code+)        (next `(require ,(p #'module))) #'code+)
;;     ))

;; (define (provide-tx code expr)
;;   (syntax-case code ()
;;     ((_ name . code+)
;;      (register-toplevel
;;       `(provide ,#'name))
;;      ((rpn-next) #'code+ expr))))

;; ;; Ignore #lang constructs
;; (define (lang-tx code expr)
;;   (syntax-case code (planet)
;;     ((_ planet path . code+) ((rpn-next) #'code+ expr))
;;     ((_ path . code+)        ((rpn-next) #'code+ expr))))


;; (define (stx->string stx)
;;   (let ((sym/str (syntax->datum stx)))
;;     (cond
;;      ((symbol? sym/str) (symbol->string sym/str))
;;      ((path? sym/str) (path->string sym/str))
;;      ((string? sym/str) sym/str)
;;      (else (error 'stx->string)))))

;; (define (path-tx code expr)
;;   (define (add-path s-p code+ [pre #f])
;;     (let ((p (stx->string s-p)))
;;       (forth-search-path
;;        (cons (if pre (simplify-path (build-path pre p)) p)
;;              (forth-search-path)))
;;       ((rpn-next) code+ expr)))
;;   (syntax-case code (staapl)
;;     ((_ staapl path . code+) (add-path #'path #'code+ staapl-dir))
;;     ((_ path . code+) (add-path #'path #'code+))
;;     ))

;; (define (stx-srcloc stx)
;;   #`(quote (#,(syntax-source stx)
;;             #,(syntax-line stx)
;;             #,(syntax-column stx)
;;             #,(syntax-position stx)
;;             #,(syntax-span stx))))


