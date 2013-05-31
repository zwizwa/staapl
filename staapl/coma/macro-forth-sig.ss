#lang scheme/base
(require scheme/unit
         "macro-forth.ss"
         "../rpn/main.ss"
         "../tools/signature-forms.ss"
         "../rpn/rpn-signature-forms.ss"
         "../forth/forth-lex.ss"
         (for-syntax
          "macro-forth-tx.ss"
          "../tools/stx.ss"
          "../tools/grabbag.ss"
          "../forth/lexer-tx.ss"
          "../rpn.ss"
          "../forth/forth-tx.ss"
          scheme/base))

(provide
 macro-forth^
 forth-lex-string/cps  ;; Needed to make `forth-compile' work.
 )

(begin-for-syntax
 
 (define (with-mode def-word register! wrap)
   (make-rpn-forth-definition-transformer
    (lambda (name)
      #`(#,def-word #,register! #,wrap #,name rpn-lambda))))

 ;; ':' takes semantics from last entry.
 (define (last-mode register! forthword wrapword macroword wrapmacro)
   (make-rpn-same-definition-transformer
    (lambda (d) ;; get-compile
      (let ((entry (d-last d)))
        (rpn-make-header->compile
         (lambda (name)
           ;; (printf "entry: ~a\n" (map syntax->datum entry))
           (syntax-case entry (macro-word)
             ((macro-word . _) #`(#,macroword #,register! #,wrapmacro #,name rpn-lambda))
             (else             #`(#,forthword #,register! #,wrapword #,name rpn-lambda)))))))))
)

;; This creates some prefix parsing words and the `forth-begin' form
;; in terms of non-parameterized macros in macro-forth.ss and some
;; parameterized behaviour captured by the mf: words below.

(define-signature macro-forth^
  (mf:reg
   mf:wrap-macro
   mf:wrap-word
   mf:wrap-variable  ;; Note this is different than label:allot from label^
   mf:compile!
   
   (define-syntax-rule (mf:forth-begin . code)
     (forth-begin/init (forth-word mf:reg mf:wrap-word #f rpn-lambda) . code))

   (define-syntaxes-ns (macro)
     (:macro :forth :variable : expand) 
     (values
      (with-mode #'macro-word #'mf:reg #'mf:wrap-macro)
      (with-mode #'forth-word #'mf:reg #'mf:wrap-word)
      (with-mode #'forth-word #'mf:reg #'mf:wrap-variable)
      (last-mode #'mf:reg
                 #'forth-word #'mf:wrap-word
                 #'macro-word #'mf:wrap-macro)
      ;; Recursive expansion.  This is necessary to make sure 'require and
      ;; 'define-syntax forms introduce transformer bindings before
      ;; continuing parsing.  This needs to serialize all dynamic context
      ;; to the code stream.
      (make-rpn-expand-transformer
       (lambda ()
         #`(mf:forth-begin #,(forth-path-dump))))))

   (prefix-parsers
    (macro)
    ((forth) (:forth #f))
    ((macro) (:macro #f))
    
    ((variable n)   (:variable n 1))  ;; Wrapper will insert `allot'
    ((2variable n)  (:variable n 2))

    
    ;; These need expand because the require form might
    ;; introduce transformer bindings.
    ((require id)      (|{| require-id spec   id |}| expand))
    ((staapl id)       (|{| require-id staapl id |}| expand))
    ((planet id)       (|{| require-id planet id |}| expand))
    ((require-file id) (|{| require-id file   id |}| expand)))


   ;; Prefix code with a path inclusion, insert macro form and
   ;; instantiate.
   (define-syntax forth-begin
     (lambda (stx)
       (syntax-case stx ()
         ((_ . code)
          #`(begin
              (mf:forth-begin . code)
              (mf:compile!))))))

   ;; For module language
   (define-syntax-rule (forth-module-begin . words)
     (#%plain-module-begin
      (forth-begin . words)))

   ;; For interaction
   (define-syntax-rule (forth-compile str)
     (forth-lex-string/cps forth-begin str))
   (define-syntax-rule (forth-load/compile str)
     (forth-begin load str))
   
   
   ))
   
    
