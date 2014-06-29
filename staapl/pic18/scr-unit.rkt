#lang racket/unit

;; Shallow Co-routines (SCR).
;;
;; An SCR is a representation of a Finite-State Machine (FSM) as a
;; task/thread with an execution stack of 1 cell.
;;
;; Some machinery is added to make the representation as small as
;; possible, using bytes (or half-bytes) when possible, saving RAM at
;; the expense of some Flash dispatch code to allow for a large number
;; of SCRs to exist on a single low-memory uC.

(require
 racket/match
 "../op.rkt"
 "../coma/macro.rkt"
 "../sig.rkt")

(import stack^ jump^ state-tag^ memory-extra^ cfg^ control^ rstack^)
(export scr^)

(define scr-tag (state-tag-label 'scr))
(define-struct scr (var labels))

;; Currently state just contains tags.
(define (scr-ref state) (state-tag-ref state scr-tag))
(define (scr-set v)     (state-tag-set scr-tag v))

(define (scr-next-id state)
  (length (scr-labels (scr-ref state))))

;; Add label to SCR jump table state.
(define (scr-update state label)
  (match (scr-ref state)
         ((struct scr (var labels))
          (scr-set (make-scr var (cons label labels))))))


(patterns
 (macro)

  ;; Begin new jump context.
 (([qw var] scr-begin)
  (scr-set (make-scr var '())))

 ;; Register label and return ID.
 (([qw label] scr-reg)
  (lambda (state)
    (let ((id (scr-next-id state)))
      ((macro: ,(scr-update state label) ',id)
       state)))))

(compositions
 (macro) macro:

 ;; FIXME: This should be an instantiated word.
 (do-yield   r> ! exit)   ;; state --  \ register next state and exit SCR

 (scr-yield  make-label dup >m ;; create jump target for registry and CFG
             scr-reg           ;; register jump target and save next ID to SCR state
             do-yield exit     ;; jump to yielder
             m> enter:)        ;; mark next entry point
 
 (scr-var ,(lambda (state)
             ((macro: ',(scr-var (scr-ref state)))
              state)))

 )
 


;; TODO:
;;   - compile entry point / jump table
;;   - share the "state ! ;" fragment in a single word.
