#lang scheme/base

;; Minimalistic REPL for a string evaluator like 'forth-command.

(require
 scheme/control  ;; for 'prompt so errors don't kill the REPL
 ;; readline/rep ;; readline command line editing (not standard on XP, so commented out)
)
(provide repl
         repl-command-hook
         repl-break-hook)

(define repl-command-hook
  (make-parameter
   (lambda (interpret str)
     (unless (equal? "" str)
       (interpret str))
     (interpret "OK"))))

(define (repl-break-exit command str)
  (printf "\nCommand \"~a\" interrupted.\nTrying cold restart...\n" str)
  (command "cold")
  (let ((ok (with-timeout 1 (lambda () (command "OK")))))
    (unless ok (printf "Timed out.\n"))
    ok))

(define (repl-break command str)
  (printf "\nCommand \"~a\" interrupted.\n" str)
  #t)

(define repl-break-hook
  (make-parameter
   ;; repl-break
   repl-break-exit))

(define (with-timeout sec thunk)
  (define retv #t)
  (define (watchdog)
    (sleep sec)
    (set! retv #f))
  (let ((ok (thread thunk))
        (timeout (thread watchdog)))
    (sync ok timeout)
    (kill-thread ok)
    (kill-thread timeout)
    retv))

(define (repl command)

  ;; User break while reading a line is ignored.
  (define (_read-line)
    (with-handlers
        ((exn:break? (lambda _ "")))
      (read-line)))

  ;; User break while running a command will reset the target device
  ;; using the "cold" command, which triggers external reset circuitry
  ;; if available.

  (define (_command cmd)
    ; (printf "executing: ~a\n" cmd)
    (prompt
     (with-handlers
         ((exn:break?
           (lambda _ ((repl-break-hook) command cmd))))
       ((repl-command-hook) command cmd)
       #t)))

  ;; Main console loop
  (define (console)
    (let ((cmd (_read-line)))
      (unless (eof-object? cmd)
        (when (_command cmd)
            (console)))))

  ;; Start.
  (with-handlers ((void void))
    (file-stream-buffer-mode (current-output-port) 'none))
  (printf "Press ctrl-D to quit.\n")
  (when (_command "")  ;; first "OK"
    (console))
  (printf "Dada.\n")
  )


