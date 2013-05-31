(module pattern-utils mzscheme

  
  (provide (all-defined))
  
  (define (process/merge-patterns meta-pattern stx)
    (apply append
           (map (lambda (p-stx)
                  (syntax->list
                   (meta-pattern p-stx)))
                (syntax->list stx))))



  )
