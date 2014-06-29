#lang scheme/base

;; Simple interface on top of forth-lex.ss using CPS macros.  This
;; allows the lexer to be used without having to resort to lowlevel
;; macros.

;; In Staapl, the convention is for CPS macros to have the macro
;; continuation as first argument.

(require
 (for-syntax
  scheme/base
  "lexer-tx.ss"))

(provide (all-defined-out))

(define-syntax (forth-lex-string/cps stx)
  (syntax-case stx ()
    ((_ compile str)
     #`(compile #,@(string->forth-syntax #'str)))))

(define-syntax (forth-lex-file/cps stx)
  (syntax-case stx ()
    ((_ compile filename)
     #`(compile #,@(file->forth-syntax
                    (syntax->datum #'filename)
                    #'filename)))))


