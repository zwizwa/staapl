#lang scheme/base

(require
 "../scat.ss"
 (for-syntax
  scheme/base))

(provide (all-defined-out))

(define-syntax-ns (pesel) +
  (lambda (stx)
    (syntax-case stx ()
      ((_ a b)
       (let ((da (syntax->datum #'a))
             (db (syntax->datum #'b)))
         (let ((na (number? da))
               (nb (number? db)))
           (if
            (and na nb)
            (datum->syntax stx (+ da db))
            #`(+ a b))))))))

;; So, yes, this is quite straightforward. The difficultie lies in
;; application of non-primitives. In a concatenative language,
;; non-primives always reduce to a concatenation of primitives, where
;; each can be applied in sequence.