#lang racket/base

;; State threading extensions based on prompt tags and reset/shift.

;; To be provided by client:
;;  * prompt tag
;;  * mix function

(require
 "../tools.rkt"
 racket/control)

(provide
 tag/pack/unpack->open/close

 make-stop
 make-stitch
 with-stitches)

;; Create a function that can be inserted into composition of unary
;; functions to peel off composition layers dynamically. Nesting these
;; will return the layers inside-out.

;; The function is wrapped by a 'stop' to provide an outer shift that
;; terminates the loop.

(define (make-stitch tag [mix values] [more #t])
  (lambda (val)
    (shift-at tag
              rest
              (values (and more rest)  ;; composable continuation
                      mix              ;; state mixer
                      val))))          ;; intermediate value

;; Stop = stitch without continuation.
(define (make-stop tag [mix values])
  (make-stitch tag mix #f))

;; Sequencer to perform hidden state threading. The tag corresponds to
;; the prompt tag used by reset/shift. The state is the data to be
;; threaded throughout the computation, which will be mixed with
;; computation.

(define (with-stitches tag fn state0 value0)
  (let next ((state state0)
             (kmv   (lambda ()
                      (reset-at tag
                                (fn value0)))))
    (let-values (((k mix value) (kmv)))
      (let-values (((state+ value+) (mix state value)))
        (if k
            (next state+ (lambda () (k value+)))
            (values state+ value+))))))

;; TEST

;; (define (mix state value)
;;   (printf "MIX: ~a ~a\n" state value)
;;   (values (- state 100)
;;           (+ value 100)))

;; (define tag (make-continuation-prompt-tag 'tag))
;; (define x add1)
;; (define y (make-stitch tag mix))
;; (define stop (make-stop tag mix))

;; (define (make-composition . fns)
;;   (apply compose (reverse fns)))

;; (with-stitches tag (make-composition x x x y x x y) 0 0)



;; HIGH LEVEL WRAPPER

;; Create open and closed functions based on pack and unpack
;; functions. This creates maps between wrapping and wrapped types.

(define (tag/pack/unpack->open tag pack unpack)
  (lambda (fn)
    (make-stitch tag
                     (lambda (extend base)
                       (unpack (fn (pack extend base)))))))

(define (tag/pack/unpack->close tag pack unpack)
  (lambda (fn)
    (lambda (e/b)
      (let-values (((e b) (unpack e/b)))
        (let-values (((e+ b+) 
                      (with-stitches tag
                                     (compose (make-stop tag) fn)
                                     e b)))
          (pack e+ b+))))))

(define (tag/pack/unpack->open/close . args)
  (values
   (apply tag/pack/unpack->open  args)
   (apply tag/pack/unpack->close args)))
                      
(define-syntax-rule (type->unpack type)
  (match-lambda
   ((struct type (e+ b+))
    (values e+ b+))))

