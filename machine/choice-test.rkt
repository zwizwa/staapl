#lang racket/base
(require "choice.rkt"
         srfi/41
         "fail.rkt"
         "enum.rkt")

(define (choice/seq seq mark?) (choice/enum (seq->enum seq) mark?))
(define (choice/range a b) (choice/seq (in-range a b) #f))
(define (choice/naturals)  (choice/seq (in-naturals) #t)) ;; marked naturals
         
(define sols
  (enum->stream
   (solutions
    (query
     (let* ((a (choice 0 1 2 3 4 5))
            (b (choice/seq (in-range 0 6) #t)))
       (assert (= 1 (modulo b 2)))
       (assert (= (+ a b) 5))
       (cut (list a b)))))))


 (stream->list (stream-take 2 sols))
