#lang racket/base

(provide (all-defined-out))
(require racket/pretty)
(require racket/match)

;; DataFlowLanguage

;; A simple single-assigment language with parallel composition.  This
;; language has no concept of sequential order (linear time), only
;; implicit partial order through statement dependency.

;; The essential idea in this _implementation_ is to use Scheme's
;; lexical scoping mechanism to construct the DFG.  Interpretation
;; happens in two stages:

;; 1. Each composite is represented by a connect functions.  Primitive
;;    connect functions register a procedure that performs an
;;    assigment after performing a function on values obtained by
;;    dereferencing nodes.

;; 2. By restarting a computation later when an operation fails, a
;;    feasible execution order can be found.

;; So composites CONNECT and primitives SCHEDULE.


;; VARIABLES

;; The basic elements are variable nodes, which can be assigned a
;; value once.
(define-struct dfl-variable (value) #:mutable)

;; Variable nodes are tracked locally for checking network properties
;; (not essential for operation).
(define (dfl-var [value #f])
  (make-dfl-variable value))

;; Variable reference + single assignment.
(define (dfl-ref n)
  (dfl-variable-value n))
(define (dfl-set! variable val)
  (set-dfl-variable-value! variable val))


;; PRIMITIVE FUNCTIONS

;; Function nodes consume values in _defined_ variable nodes and place
;; result values in _undefined_ variable nodes.  If a function has
;; undefined inputs, it is not runnable.

;; DFL is essentially parallel.  Here we use a simple algorithm to
;; construct a serialization, which allows a DFL graph to be
;; implemented as a Scheme procedure.

;; Apply any scheme function to a collection of values stored in
;; variables, storing the result.  Returns true if dependencies are
;; met, in which case the output could be set.
(define (dfl-apply fn ins outs)
  (let ((vins (map dfl-ref ins))) ;; will abort #f on undefined refs
    (for ((o outs)
          (v (call-with-values
                 (lambda () (apply fn vins))
               list)))
      (dfl-set! o v)))
  #t) ;; success


;; Check if the code is runnable through abstract interpretation: if
;; inputs are defined mark outputs as defined and inputs as used.
(define (dfl-runnable!? ins outs)
  (define (mark! var)
    (when (dfl-ref var) (error 'dfl-multiple-assignment))
    (dfl-set! var 0))
  (define (rc+!  var)
    (unless (dfl-ref var) (error 'dfl-undefined))
    (dfl-set! var (add1 (dfl-ref var))))
  (define (inputs-ready?)
    (for/and ((i ins)) (dfl-ref i)))
  (and (inputs-ready?)
       (begin
         (for-each rc+!  ins)
         (for-each mark! outs)
         #t)))


;; COMPOSITION

;; There is an interesting observation to make here: when building a
;; network with primitive and composite operations, do you want to
;; eventually "flatten" everything into a sequence of primitives, or
;; do you allow composite functions to be separately computable?  The
;; former is akin to macro-expansion, while the latter is akin to
;; function abstraction.

;; I'm inclined to go for the latter, but of course it is absolutely
;; arbitrary _how_ the execution mechanism is implemented.  Currently
;; I'm only interested in running it in Scheme, but later, when
;; compiling to C or llvm, this could be changed into a flat structure
;; to allow for more aggressive optimization.


;; The first pass transforms the syntax into a data structure
;; representing the dependency graph.  This is used to sequence the
;; statements based on the variable dependencies.
(define-struct dfl-node (in out))
(define-syntax-rule (dfl-graph
                     ((in ...) (out ...) (tmp ...))
                     (statement si so) ...)
  (let ((in  (dfl-var 0)) ...
        (out (dfl-var)) ...
        (tmp (dfl-var)) ...)
    (list (make-dfl-node (list . si) (list . so)) ...)))


;; This function takes a graph (instantiated using `dfl-node') and a
;; list of syntax objects representing its code form, and produces a
;; sorted body that can be performed by a serial program.  This
;; function is used in the expansion of `dfl-compose'.

(define (dfl-sort-graph nodes body-stx-lst)
  (let ((subprogs (apply vector body-stx-lst))
        (sequence '()))
    (let sweep! ()
      (let ((progress sequence))
        (for ((n nodes)
              (p subprogs)
              (i (in-naturals))
              #:when p)
          (match n ((struct dfl-node (in out))
                    (when (dfl-runnable!? in out)
                      (vector-set! subprogs i #f)
                      (set! sequence (cons p sequence))))))
        (unless (eq? progress sequence) (sweep!))))
    (unless (= (length sequence) (length nodes))
      (error 'dfl-undefined-refs subprogs))
    (reverse sequence)))



;; A composition (a network of data flow of operations) is abstracted
;; as a Scheme function: all parallelism is lost here to come at a
;; simple representation of recursive networks.  This macro requires
;; the statements in the correct order.
(define-syntax-rule (dfl-sequence
                     ((in ...) (out ...) (tmp ...))
                     (statement si so) ...)
  (lambda (in ...)
    (let ((in  (dfl-var in)) ...
          (out (dfl-var)) ...
          (tmp (dfl-var)) ...)
      (begin (dfl-apply statement (list . si) (list . so)) ...)
      (apply values (map dfl-ref (list out ...))))))
        
      

;; The representation of the dependency graph is used (at compile
;; time) to find a (static) ordering of statements.  Reflection is
;; used because this macro used Scheme's lexical structure to build
;; the graph.  After the ordering is known, the original syntax can be
;; translated into a sequential program.

;; Allow compile time evaluation through anchoring this module's namespace.
; (define-namespace-anchor dfl-nsa)
; (define dfl-ns (namespace-anchor->namespace dfl-nsa))



;; (define-syntax (with-macro-expand 





;; (test)

