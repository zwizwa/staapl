#lang racket/base
(require data/queue
         racket/match)


;; Actor evalutation.

;; Note sure where this is going next.  The idea is to distill state
;; machine DAGs from the collection of events that can be passed into
;; an actor network.  My guess is that with proper representation,
;; some of the control flow (if + function call) should be possible to
;; reconstruct.

;; However, it might be necessary to open up the representation of
;; lambda expressions.

(define-struct process (mailbox handlers) #:mutable #:transparent)

(define (send pid msg)
  (enqueue! (process-mailbox pid) msg))

(define self (make-parameter #f))

(define processes (make-parameter (make-queue)))

;; All tasks run until they block in a receive statement.
(define (receive/suspend alist)
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
                     (lookup '_  handlers)
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


;; Allow sequential code to continue after a receive block.

;; Captures continuation in a thunk so it can be shared across
;; branches of the message dispatch

;; Note that this implementation has leaky continuations due to
;; improper tail calls being used.  This is OK for finite runs, which
;; is the whole idea.

(define-syntax actor-begin
  (syntax-rules (receive)
    ((_) (void))
    ((_ (receive ((tag . args) body ...) ...) . rest)
     (let ((rest-thunk (actor-lambda () . rest)))
       (receive/suspend
        `((tag . ,(actor-lambda args
                    body ...
                    (rest-thunk)))
          ...))))
    ((_ statement . rest)
     (begin
       statement
       (actor-begin . rest)))))

(define-syntax-rule (actor-lambda args . body)
  (lambda args
    (actor-begin . body)))





;; ------------- TEST --------------


(define (test1)
  (letrec
      ((alice
        (spawn
         (lambda ()
           (printf "alice boot\n")
           (send (bob) '(hi from-alice))
           (receive/suspend
               `((hi . ,(lambda args
                          (printf "got hi ~a\n" args)
                          (receive/suspend
                              `((_ . ,(lambda args (error 'alice-not-reached))))))))))
         '()))
       (bob
        (spawn
         (lambda ()
           (printf "bob boot\n")
           (receive/suspend
               `((hi .
                     ,(lambda args
                        (printf "got hi ~a\n" args)
                        (send (alice) '(hi from-bob))
                        (receive/suspend
                            `((_ . ,(lambda args (error 'bob-not-reached))))))))))
         '())))
    (run)))



;; same but sugared
(define (test2)
  (letrec
      ((alice
        (spawn
         (actor-lambda ()
           (printf "alice boot\n")
           (send (bob) '(hi from-alice))
           (receive
               ((hi . args)
                (printf "alice got hi ~a\n" args)))
           (receive
               ((_)
                (error 'alice-not-reached))))
         '()))
       (bob
        (spawn
         (actor-lambda ()
           (printf "bob boot\n")
           (receive
               ((hi . args)
                (printf "bob got hi ~a\n" args)
                (send (alice) '(hi from-bob))))
           (receive
               ((_)
                (error 'bob-not-reached))))
         '())))
    (run)))
  



(test2)
       
       
