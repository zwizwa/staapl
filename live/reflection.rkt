#lang racket/base

;; The interactive code is decoupled from the compiler using a
;; namespace interface.  This module provides reflective operations on
;; this namespace.

;; Note that (some) target words are accessible through this
;; namespace, but we'll use the central repository to access them.
;; The namespace is mainly intended for hosting macros and debugging
;; + interaction functions.

(require
 racket/pretty
 racket/control
 racket/match
 "../tools.rkt"
 "../code.rkt"
 "repl.rkt"
 "../ns.rkt"
 "../scat.rkt"
 "../target.rkt")

(provide (all-defined-out))


;; Code labels

(define (target-byte-addr address realm)
  ((eval 'target-byte-address) address realm))

(define (word-not-found? ex)
  (and (pair? ex)
       (eq? (car ex) 'target-word-not-found)))

;; This might seem roundabout, but we don't know here whether
;; addresses are words or bytes.  This is implemented by
;; `target-byte-addr'.
(define (target-find sym [error-thunk
                          (lambda ()
                            (raise `(target-word-not-found ,sym)))])
  (match (code-find sym)
         (#f (error-thunk))
         ((list name realm address)
          (values realm (target-byte-addr address realm)))))



(define (target-find-realm sym wanted-realm)
  (with-handlers ((word-not-found?
                   (lambda _ #f)))
    (let-values (((realm addr) (target-find sym)))
      (and (eq? realm wanted-realm) addr)))

  )

(define (target-find-code sym)  (target-find-realm sym 'code))
(define (target-find-data sym)  (target-find-realm sym 'data))

;; Macro constants
(define (macro-constant . code)
  (eval `(with-handlers ((void (lambda _ #f)))
           (state->value
            ((macro: ,@code) (init-state))
            (ns (op ? qw))))))

(define (target-words-set! words)
  ;; Write to dictionary.
  (for-each*
   (lambda (name realm address)
     (let ((word
            (eval
             `(new-target-word #:name ',name
                               #:realm ',realm
                               #:address ,address))))
       (eval
        `(begin
           (ns (target) (define ,name ,word))
           (ns (macro)  (define ,name
                          ,(case realm
                             ((code) `(scat: ',word compile))
                             ((data) `(scat: ',word literal)))))))))
   words))
  

;; The function above doesn't really do what we want.  If the recorded
;; dictionary gets out of sync with the target code, we should really
;; just recompile the kernel.  Maybe this routine could perform a test
;; and call the above routine if there is a problem?

;; FIXME: implement word address check.

(define (target-words-check! words)
  (void))




;; Use a dict provided by (target-words) to perform a reverse lookup.
(define (reverse-lookup dict realm address)
  (prompt
   (for-each*
    (lambda (name r a)
      (when (and (eq? r realm)
                 (eq? a address))
        (abort name)))
    dict) #f))



;; Generate the project state file as an expression that can be
;; evaluated in a namespace containing this file.
;;(define (dict-snapshot)
;;  `(begin
;;     (target-words-set!   ',(code-labels))
;;     (code-pointers-set!  ',(code-pointers))))


;; start console
;;
;; disappearing.

(define (run
         [startup void])
  (define (con) (eval `((comm-reconnect))))
  (define (dis) (eval `((comm-close))))

  (define (with-connection thunk)
    (with-handlers
        ((void (lambda (ex)
                 (let ((ex-pretty
                        (cond
                         ((not (exn? ex)) ex)
                         ((exn:fail:filesystem? ex) "Can't open console device.")
                         (else (exn-message ex)))))
                   (printf "ERROR: ~a\n" ex-pretty))
                 (dis)
                 #f)))
      (con)
      (thunk)
      (dis)
      #t))
  
  ;; Retry startup code until success.
  (define (run-startup)
    (unless (with-connection startup)
      (sleep 1)
      (run-startup)))
  (run-startup)
  
  ;; Start mainloop
  (repl (lambda (cmd)
          ;; (printf "cmd: ~a\n" cmd)
          (with-connection
           (lambda ()
             (eval `(forth-command ,cmd))))))
  )


;; Code garbage collection.

;; Instead of the incremental, 'dump all' model used in incremental.rkt
;; it is also possible to perform whole-program reachability analysis
;; by constructing a serialized code graph from a number of entry
;; nodes (in case of a microcontroller: the reset + interrupt
;; vectors).

;; FIXME: this does require those vectors to be accessible using
;; names.


;; Load a dictionary file.  See staaplc.rkt for an explanation of the
;; structure.

(define (read-dictionary [port (current-input-port)])
  (read-line port) ;; skip the #lang header
  (values (read) (read) (read)))

(define (load-dictionary file)
  (let-values (((info reqs init)
                (with-input-from-file file read-dictionary)))
    (eval info)
    (eval reqs)
    (eval init)))


(define (print-tword sym)
  (eval `(print-target-word (ns (target) ,sym))))


(define (target-compile-macro sym)
  (let ((macro (eval `(ns (macro) ,sym))))
    ;; (printf "macro: ~s ~s\n" sym macro)
    (eval `(target> : ,sym ',macro compile exit))))

;; Used for target->host RPN.  Like target-interpret but without all
;; the target dictionaries.  FIXME: this should not be here but in
;; reflection.

(define (host-interpret sym)
  (define defined? (make-ns-defined? sym))
  (cond ((defined? '(host)) => (lambda (x) x))
        (else (eval `(live: ,sym)))))



;; Compile forth code.

;; Log interactive state.  Because it's impossible to serialize the
;; the state built up during interactive compilation, we provide a way
;; to log the subset of the evaluations that lead to definitions, so
;; they can be replayed on next start.

(define eval-log-file
  (make-parameter "staapl.log"))

(define forth-namespace (make-parameter #f))
(define forth-begin-prefix (make-parameter '()))

(define (eval-forth forth-expr [log #t])
  ;; Eval before logging in case there is an error.
  (eval
   `(forth-begin ,@(forth-begin-prefix) ,@forth-expr)
   (or (forth-namespace) (current-namespace)))
  (when log
    (with-output-to-file
        (eval-log-file)
      (lambda () (write forth-expr) (newline))
      #:exists 'append)))
