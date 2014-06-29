#lang scheme/base

;; poke - a forth vm

;; this is to serve as the new core for PF, an maybe a kind of unix
;; shell. emphasis is on small size and simplicity:

;; * uses linear lists as basic data structure, no vectors. (see also linear.ss)
;; * primitives are C functions taking a single argument, the vm state.

(require
 "../tools.ss"
 "cgen.ss")

;; extend standard c generator


(expressions

 ((S) #`(-> s S))   ;; data stack
 ((F) #`(-> s F))   ;; free list
 ((R) #`(-> s R))   ;; return stack

 ((car x) #`(index x 0))
 ((cdr x) #`(index x 1))
  
 ((top)   #`(car (S)))
 ((2nd)   #`(car (cdr (S))))

 )


(statements

 ;; primitive linear operations (permutations)
 ((cycle2 a b)
  #`(bind
     (((typeof a) t a))
     (= a b)
     (= b t)))

 ((cycle3 a b c)
  #`(bind
     (((typeof a) t a))
     (= a b)
     (= b c)
     (= c t)))

 ;; move cell from one list to another
 ((move from to)
  #`(cycle3 (cdr from) to from))

 ;; primitive C functions
 ((word name . body)
  #`(def ((void name) (state_t s)) . body))

 ;; during bootstrapping, nothing is managed, and drop will not care
 ;; about data types.

 ((lit x) #`(statements (save) (= (top) x)))

 ;; minimal compiler
 ((forth . lst)
  #`(statements
     #,@(map-stx (lambda (atom)
                   (if (identifier? atom)
                       #`(#,atom)
                       #`(lit #,atom)))
                 #'lst)))
    
  

 )


;; macro for loading poke files
(define-sr (poke statement ...)
  (display
   (statement->string
    #'(statements statement ...))))

(poke

 (word save (move (F) (S)))
 (word drop (move (S) (F)))
 

 (word hello (forth 1 2 3))
 
 (top)
 (2nd)
 
 )

;; i'm thinking about using linked lists instead of cons cells, since
;; i can't seem to make sense of bootstrapping the type system. (it
;; looks like i need to solve both list + type system at the same
;; point for efficiency..)

;; some remarks:

;; * if there is no lowlevel access (raw pointers) then it doesn't
;;   matter how big the integers are.

;; tagged types:
;; - integers    2:
;; - floats      0: 
;; - cons cells  1: (-1 +3)

;; objects:
;; - symbols
;; - arrays
;; - anything else

;; implementation is at least 32 bit, objects are word aligned, this
;; gives 2 tag bits, 3 special types.



;; maybe it's best to start with the machine in baker's paper:
;; this only uses cons cells and atoms (symbols)

;; http://home.pipeline.com/~hbaker1/LinearLisp.html


