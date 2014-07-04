;; from syntax/module-reader

(module module-reader racket/base
  (provide
   (rename-out
    [provide-module-reader #%module-begin]
    [wrap wrap-read-all]))

  (require
   racket/runtime-path
   racket/pretty
   "../tools.rkt"
   "lexer-tx.rkt")
   
  
  (define-syntax provide-module-reader
    (syntax-rules ()
      [(_ lib)
       (#%module-begin
         (#%provide (rename *read read)
                    (rename *read-syntax read-syntax))
         
         (define (*read in)
           (wrap 'lib in read-forth))
         
         (define (*read-syntax src in)
           (wrap 'lib in (lambda (in)
                           (read-forth-syntax src in)))))]))

  ;; The module language's location is interpreted relative to the
  ;; staapl collects dir, since we can't use absolute paths.
  (define-runtime-path _staapl "..")
  (define staapl (path->string (simplify-path _staapl)))

  ;; Read a whole module expression using provided atom reader.
  (define (wrap lib port read)
    (let ([lib-path
           (format "~a~a" staapl lib)]
          [body
           (let loop ([a null])
             (let ([v (read port)])
               (if (eof-object? v)
                   (reverse a)
                   (loop (cons v a)))))])
      ;; (printf "module language : ~a\n" lib-path)
      (let* ([p-name (object-name port)]
             [name (if (path? p-name)
                       (let-values ([(base name dir?) (split-path p-name)])
                         (string->symbol (path->string (path-replace-suffix name #""))))
                       'page)])
        (let ((form
               `(module ,name (file ,lib-path)
                  . ,body)))
          ;; (for ((b body)) (printf "~s ~s\n" (syntax->datum b) (syntax-source b)))
          ;; (pretty-print (syntax->datum (datum->syntax #f form)))
          form)))))

