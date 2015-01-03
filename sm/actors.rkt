#lang racket/base
(require data/queue
         racket/match)

(define any '_)

(define-struct process (mailbox handlers) #:mutable #:transparent)

(define (send pid msg)
  (enqueue! (process-mailbox pid) msg))

(define self (make-parameter #f))

(define processes (make-parameter (make-queue)))

;; All tasks run until they block in a receive statement.
(define (receive alist)
  (set-process-handlers! (self) alist))
  
;; Dumb scheduler.  Run the whole queue once.
(define (tick)
  (for ((p (queue->list (processes))))
    (self p)
    (tick-process p)))

(define (lookup tag handlers)
  (let ((pair (assoc tag handlers)))
    (and pair (cdr pair))))

(define (tick-process p)
  (match p
    ((struct process (mailbox handlers))
     (when (> (queue-length mailbox) 0)
       (match (dequeue! mailbox)
         ((list-rest tag body)
          (let ((fn
                 (or (lookup tag handlers)
                     (lookup any handlers)
                     (error `(message-not-handled ,tag ,body)))))
            (apply fn body))))))))

;; Run everything until there are no more messages in the mailboxes.
(define (nb-messages)
  (for/fold
      ((n 0))
      ((p (queue->list (processes))))
    (+ n (queue-length (process-mailbox p)))))

(define (run)
  (let again ()
    (tick)
    (unless (zero? (nb-messages)) (again))))




(define (spawn fn args)
  (let* ((handlers `((__boot__ . ,(lambda _ (apply fn args)))))
         (mailbox (make-queue))
         (process (make-process mailbox handlers)))

    ;; Wait for boot until first scheduler run to allow a collection
    ;; of cross-linked tasks to be spawned.
    (enqueue! mailbox '(__boot__))
    (enqueue! (processes) process)
    ;; PIDs are wrapped in thunks to allow cross-linking with letrec.
    (lambda () process)))



;; ------------- TEST --------------


(define (test)
  (letrec
      ((alice
        (spawn
         (lambda ()
           (printf "alice boot\n")
           (send (bob) '(hi from-alice))
           (receive
               `((hi . ,(lambda args
                          (printf "got hi ~a\n" args)
                          (receive
                              `((_ . ,(lambda args (error 'alice-not-reached))))))))))
         '()))
       (bob
        (spawn
         (lambda ()
           (printf "bob boot\n")
           (receive
               `((hi .
                     ,(lambda args
                        (printf "got hi ~a\n" args)
                        (send (alice) '(hi from-bob))
                        (receive
                            `((_ . ,(lambda args (error 'bob-not-reached))))))))))
         '())))
    (run)))

(test)
