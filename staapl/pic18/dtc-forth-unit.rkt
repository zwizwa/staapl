#lang scheme/unit
(require  "../coma/macro-forth-sig.ss"
          scheme/pretty)
(import)
(export macro-forth^)

(define (x . args)
  (pretty-print args)
  (void))
         

(define mf:compile!      x)
(define mf:reg           x)
(define mf:wrap-macro    x)
(define mf:wrap-word     x)
(define mf:wrap-variable x)

 
