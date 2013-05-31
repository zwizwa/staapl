#lang scheme/base

;; All identifier mapping (prefix) code is hidden behind ns.ss and
;; ns-tx.ss code.

(provide
 ns-tx
 ns-prefixed
 name->identifier)

(require
 scheme/pretty
 "tools/stx.ss"
 (for-template
  scheme/unit
  scheme/base))

    

(define ns-separator "/")

(define (ns->prefix-string ns-lst)
  (apply string-append
         (map (lambda (x) (format "~a~a" x ns-separator))
              ns-lst)))

(define (ns->prefix ns-stx)
  (ns->prefix-string
   (syntax->datum ns-stx)))

;; Forth macros are identified by symbols. For convenience strings,
;; are converted to identifier preserving lexical context and source
;; location.

(define (name->identifier stx)
  (let ((name (syntax->datum stx)))
    (cond
     ((symbol? name) stx)  
     ((string? name) (datum->syntax stx (string->symbol name) stx stx))
     (else
      (raise-syntax-error #f "ns form contains non-identifier (non-string)" stx)))))



;; Recertify in terms of original syntax.  Note that prefix-id doesn't recertify.
(define (ns-prefixed ns name)
  (syntax-recertify
   (prefix-id (ns->prefix ns)
              (name->identifier name))
   name (current-code-inspector) #f))


;; The syntax-property stuff doesn't work yet.  The point is to get
;; check-syntax to do the right thing..

;; Catch-all transformer.
(define (ns-tx stx)
  (syntax-case stx ()
    ((_ (namespace ... name))
     (ns-tx #`(_  (namespace ...) name)))
    ((_ (namespace ...) expr)
     (let* ((prefixed
             (lambda (n)
               (ns-prefixed #'(namespace ...) n)))
            (prefixed-list
             (lambda (stx)
               (map prefixed (syntax->list stx))))
            (prefixed-list-list
             (lambda (stx)
               (map prefixed-list (syntax->list stx))))
            (prefixed-letform
             (lambda (p)
               (lambda (binders)
                 (for/list ((b (syntax->list binders)))
                   (syntax-case b ()
                     ((n e) #`(#,(p #'n) e))))))))
       (if (identifier? #'expr)
           ;; REFERENCE
           (let ((id (prefixed #'expr)))
             (let ((sym (syntax->datum id))
                   (b (identifier-binding id)))
               ;; (unless b
               ;;   (raise-syntax-error #f "undefined variable after mangling" stx id))
               ;; (printf "~a : ~a\n" sym b)
               id))
           (let ((form?
                  (let ((form (car (syntax->datum #'expr))))
                    ;; (printf "form = ~a\n" form)
                    (lambda (symb) (eq? form symb)))))

             (syntax-case #'expr (extends)

               ;; BINDING
               ((form (name . a) e)
                (or (form? 'define)
                    (form? 'define-syntax))
                #`(form (#,(prefixed #'name) . a) e))
               
               ((form name e)
                (or (form? 'define)
                    (form? 'define-syntax))
                #`(form #,(prefixed #'name) e))

               ((form names e)
                (or (form? 'define-values)
                    (form? 'define-syntaxes))
                #`(form #,(prefixed-list #'names) e))

               ((form binders e)
                (or (form? 'let)
                    (form? 'letrec)
                    (form? 'shared))
                #`(form #,((prefixed-letform prefixed) #'binders) e))
               
               ((form binders e)
                (or (form? 'let-values)
                    (form? 'letrec-values))
                #`(form #,((prefixed-letform prefixed-list) #'binders) e))

               ;; MODULE
               ((form . specs)
                (or (form? 'combine-out))
                #`(form #,@(for/list ((spec (syntax->list #'specs)))
                             (ns-tx #`(ns (namespace ...) #,spec)))))

               ((form spec (old new) ...)
                (or (form? 'rename-in))
                #`(form spec #,@(prefixed-list-list #'((old new) ...))))

               ((form spec name ...)
                (or (form? 'except-in))
                #`(form spec #,@(prefixed-list #'(name ...))))

               ;; UNIT
               ((form unit-name extends sig^ (name ...))
                (or (form? 'define-signature))
                #`(form unit-name extends sig^ #,(prefixed-list #'(name ...))))

               ;; UNIT
               ((form unit-name (name ...))
                (or (form? 'define-signature))
                #`(form unit-name #,(prefixed-list #'(name ...))))
               
               

;; THIS JUST NEEDS PORTING               
;;                     ((_ unit-name (sig-id ...))
;;                      (id-in? #'define-signature)
;;                      #`(#,form-id unit-name
;;                                   #,(for/list ((si (in-stx #'(sig-id ...))))
;;                                       (syntax-case si (define-syntaxes)
;;                                         ((define-syntaxes (name ...) expr)
;;                                          #`(define-syntaxes #,(prefixed-list #'(name ...)) expr))
;;                                         ((other-form ...)
;;                                          #`(other-form ...))  ;; might need extension later..
;;                                         (id
;;                                          (prefixed #'id))))))
                              

               )))))))


;; *** postprocessor ***

;; The implementation with 'expand-to-top-form gives obscure errors
;; from time to time, producing identifiers that are not visible in
;; the caller's namespace for reasons unknown to me.

;; While it is a bit tedious to spell out all possible forms to be
;; transformed by the ns mechanism, doing so seems a lot better than
;; having to deal with macro system internals that are exposed by
;; calling the expander directly.

;; ;; Catch-all transformer       
;; (define (ns-tx stx)
;;   (syntax-case stx ()
;;     ((_ (namespace ... name))
;;      (ns-tx #`(_  (namespace ...) name)))
;;     ((_ (namespace ...) expr)
;;      (let* ((prefixed
;;              (lambda (n)
;;                (ns-prefixed #'(namespace ...) n)))
;;             (prefixed-list
;;              (lambda (stx)
;;                (map prefixed (syntax->list stx))))
;;             (prefixed-list-list
;;              (lambda (stx)
;;                (map prefixed-list (syntax->list stx))))
;;             (prefixed-binders
;;              (lambda (p)
;;                (lambda (binders)
;;                  (for/list ((b (syntax->list binders)))
;;                    (syntax-case b ()
;;                      ((n e) #`(#,(p #'n) e))))))))
;;        (if (identifier? #'expr)
;;            ;; REFERENCE
;;            (prefixed #'expr)
;;            (let ((form?
;;                   (let ((form (car (syntax->datum #'expr))))
;;                     (lambda (symb) (eq? form symb)))))

;;              (syntax-case #'expr ()

;;                ;; MODULE
;;                ((form . specs)
;;                 (form? 'combine-out)
;;                 #`(form #,@(for/list ((spec (syntax->list #'specs)))
;;                              (ns-tx #`(ns (namespace ...) #,spec)))))

;;                ((form spec (old new) ...)
;;                 (form? 'rename-in)
;;                 #`(form spec #,@(prefixed-list-list #'((old new) ...))))

;;                ((form spec name ...)
;;                 (form? 'except-in)
;;                 #`(form spec #,@(prefixed-list #'(name ...))))


;;                ;; BINDINGS
               
;;                ;; Here it's easier to expand the top form to reveal
;;                ;; primitive binding forms, since there are many forms
;;                ;; that bind names.
;;                (else
;;                 (let* ((form     (expand-syntax-to-top-form #'expr))
;;                        (form-sym (syntax->datum (car (syntax->list form))))
;;                        (form-id  #f)
;;                        (id-in?    (lambda ids
;;                                     (for/or ((id ids))
;;                                             (set! form-id id)
;;                                             (eq? form-sym (syntax->datum id))))))

;;                   (pretty-print (syntax->datum form))

;;                   (unless (symbol? form-sym)
;;                     (raise-syntax-error #f "bad ns form" stx))
;;                   (syntax-case form ()
               
;;                     ((_ (name . formals) . body)
;;                      (id-in? #'define
;;                              #'define-syntax)
;;                      #`(#,form-id (#,(prefixed #'name) . formals) . body))
                    
;;                     ((_ name expr)
;;                      (id-in? #'define
;;                              #'define-syntax)
;;                      #`(#,form-id #,(prefixed #'name) expr))

;;                     ((_ (name ...) expr)
;;                      (id-in? #'define-values
;;                              #'define-syntaxes)
;;                      #`(#,form-id #,(prefixed-list #'(name ...)) expr))
                    
;;                     ((_ binders . body)
;;                      (id-in? #'let-values
;;                              #'let*-values
;;                              #'letrec-values)
;;                      #`(#,form-id #,((prefixed-binders prefixed-list) #'binders) . body))

;;                     ((_ unit-name (sig-id ...))
;;                      (id-in? #'define-signature)
;;                      #`(#,form-id unit-name
;;                                   #,(for/list ((si (in-stx #'(sig-id ...))))
;;                                       (syntax-case si (define-syntaxes)
;;                                         ((define-syntaxes (name ...) expr)
;;                                          #`(define-syntaxes #,(prefixed-list #'(name ...)) expr))
;;                                         ((other-form ...)
;;                                          #`(other-form ...))  ;; might need extension later..
;;                                         (id
;;                                          (prefixed #'id))))))
               
;;                     (else
;;                      (begin
;;                        (printf "WARNING: ns-tx doesn't match top form:\n")
;;                        (pretty-print (syntax->datum form))
;;                        (pretty-print (syntax->datum (expand-to-top-form form)))
;;                        #'expr))))))))))))


;;   ;; Notes:
;;   ;;
;;   ;; * It's not ok to insert the toplevel form's identifier back into
;;   ;;   the syntax object, since its bindings are (probably) not
;;   ;;   visible in the user's context.  I'm using the bindings visible
;;   ;;   from here.
;;   ;;
;;   ;; * For some reason including identifiers in the syntax-case
;;   ;;   literal list won't match them properly, so I use guards that
;;   ;;   match the symbol instead.
  
