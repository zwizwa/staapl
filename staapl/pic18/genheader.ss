#lang scheme/base
(require scheme/match
         scheme/dict)

; Parse an MPASM INC file and spit out the constants as a Scheme
; module.

; I'm using a file-based approach instead of parsing the file from a
; macro, as I guess it's not allowed to distribute the original
; headers.  In a compiled form this shouldn't be a problem.

; Quick and dirty.  I didn't feel like thinking too much..
(define (parse-inc port dict [printf void])
  (define (value str)
    (match (regexp-match #rx"H'(.*)'" str)
           ((list _ hex)
            (format "#x~a" hex))
           (else str)))
  
  (define (equ command comment)
    (match (regexp-split #rx" +EQU +" command)
           ((list name val)
            (let ((pval (value val))
                  (sname (string->symbol name)))
              (printf "(~a ~a) ; ~a\n" name pval comment)
              (dict-set! dict sname pval)))
           (else
            (if (zero? (string-length command))
                (printf "; ~a\n" comment)
                (printf "; NOT PARSED: ~a ; ~a\n" command comment)
                ))))
    
    (for ((line (in-lines port)))
    (match (regexp-split #rx" *; *" line)
           ((list-rest command comment)
            (equ command (apply string-append comment)))
           (else
            (error 'line-parse-error line)
            (void)))))



(define (print-dict dict)
  (printf "#lang scheme/base\n")
  (printf "(require staapl/pic18/define-constants)\n")
  (printf "(provide (all-defined-out))\n")
  (printf "(define-pic18-const-unit\n")
  (printf "   pic18-const^\n")
  (printf "   pic18-const-id^\n")
  (printf "   pic18-const@\n")
  (for (((k v) (in-dict dict)))
    (printf "(~a ~a)\n" k v))
  (printf ")\n"))

(define (process-inc header)
  (define dict (make-hash))
  (parse-inc (open-input-file header) dict)
  (print-dict dict))

(define (test)
  (define dict (make-hash))
  (define (mplab inc)
    (string-append "/opt/xc/mplab/8.60/MPASM Suite/" inc))
  (define header (mplab "P18F2550.INC"))
  (process-inc header))

(define (script)
  (let ((args (current-command-line-arguments)))
    (when (zero? (vector-length args))
      (error 'no-script-args))
    (let ((filename (vector-ref args 0)))
      (process-inc filename))))

(script)


  



