#lang scheme/base

;; The interactive code is decoupled from the compiler using a
;; namespace interface.  This module provides reflective operations on
;; this namespace.

;; Note that (some) target words are accessible through this
;; namespace, but we'll use the central repository to access them.
;; The namespace is mainly intended for hosting macros and debugging
;; + interaction functions.

(require
 scheme/pretty
 scheme/control
 scheme/match
 "../tools.ss"
 "../code.ss"
 "repl.ss"
 "../ns.ss"
 "../scat.ss"
 "../target.ss")

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
(define (run [startup void])
  (dynamic-wind
    void
    (lambda ()
      (with-handlers
          ((void (lambda (ex)
                   (printf "Console startup failed:\n~a\n" ex)
                   (printf "Continuing with REPL anyway:\n"))))
                              
        (startup))
      (repl (lambda (cmd)
              (eval `(forth-command ,cmd)))))
    (lambda ()
      ;; (printf "Closing console.\n")
      (eval '((comm-close)))))) ;; pk2 needs proper shutdown


;; Code garbage collection.

;; Instead of the incremental, 'dump all' model used in incremental.ss
;; it is also possible to perform whole-program reachability analysis
;; by constructing a serialized code graph from a number of entry
;; nodes (in case of a microcontroller: the reset + interrupt
;; vectors).

;; FIXME: this does require those vectors to be accessible using
;; names.


;; Load a dictionary file.  See staaplc.ss for an explanation of the
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
