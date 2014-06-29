#lang scheme/base

;; CPL: A local Constraint Propagation Language with staged control
;; flow, viewed as a generalization of a (directional) data flow
;; language.

;; This solves two problems:
;;   - build abstractions over guaranteed static scheduling
;;   - modularity using Scheme's lexical scope (lambda & macros)




;; [1] http://www.cs.rice.edu/~taha/publications/conference/emsoft04.pdf
;; ``A Methodology for Generating Verified Combinatorial Circuits''


;; Remarks:
;;
;; * How useful is local constraint propagation from a perspective of
;; linear equations?  (Not very: those are global).
;;
;; * How useful is control staging?  For DFL languages it performs
;;   sequencing.  For constraint propagation languages it performs
;;   sequencing and directionalization.


(provide spec->sequence
         (struct-out node) node-value node-assert!
         (struct-out rule-class)
         (struct-out defined)
         (struct-out undefined)
         emit
         )

;; PRIMITIVE RULES
(define-struct rule-class (behaviour name))


;; THREADED STATE

;; To simplify the formulation of the algorithms (purely functional
;; monadic style is cumbersome in Scheme) some of the state is
;; implicitly threaded using dynamic variables.  For backtracking
;; purposes this state can then be saved together with the program's
;; control state.

(define code-stack (make-parameter #f))
(define value-stack (make-parameter #f))
(define (ppush! param val)
  (param (cons val (param))))

(require "choice.ss")



;; Sequential code generation
(define (code-stack-print)
  (for ((c (reverse (code-stack))))
    (display (syntax->datum c)) (newline)))
(define (emit stx)
  (ppush! code-stack stx))

;; Value bindings
(define (bind name value)
  (ppush! value-stack (cons name value)))
(define (node-value node)
  (let ((rec (assoc node (value-stack))))
    (if rec (cdr rec) (make-undefined))))
(define (set-node-value! node value)
  (value-stack (cons (cons node value) (value-stack))))
  



;; During propagation, the current code output and name . value
;; association 


;; DATA STRUCTURES


;; A `rule' is an instance of a class which determines the behaviour
;; (action), while the instance determines connectivity to a node
;; network.
(define-struct rule (action nodes))
(define-struct node (name rules) #:mutable)
(define (node-register-rule! node rule)
  (set-node-rules! node (cons rule (node-rules node))))
(define (rule-values r) (map node-value (rule-nodes r)))
(define (rule-bang! rule)
  ((rule-class-behaviour (rule-action rule))
   (rule-nodes rule)))

;; Node network = collection of nodes
(define (make-node-net names)
  (for/list ((n names)) (make-node n '())))
(define (net-find-node net key)
  (if (null? net) #f
      (let ((r (car net)))
        (if (bound-identifier=? key (node-name r))
            r (net-find-node (cdr net) key)))))
(define (net-nodes net) net)
(define (node->string node)
  (format "~a = ~a"
            (syntax->datum (node-name node))
            (node-value node)))
(define (net-print net)
  (for ((n net))
    (display (node->string n))
    (newline)))
(define (net-constrain! net rule-class node-names)
  (let* ((nodes (for/list ((n node-names)) (net-find-node net n)))
         (rule (make-rule rule-class nodes)))
    (for ((node nodes)) (node-register-rule! node rule))
    rule))
(define (net-assert! net var [value (make-defined)])
  (node-assert! (net-find-node net var) value))
(define (net-float! net var)
  (set-node-value! (net-find-node net var) (make-undefined)))
(define (net->rules net)
  (define h (make-hash))
  (for ((n (net-nodes net)))
    (for ((r (node-rules n)))
      (hash-set! h r #t)))
  (for/list ((k (in-hash-keys h))) k))
    






;; CONTROL FLOW STAGING

;; The constraint network is evaluated at compile using a local
;; propagation algorithm.  Node values carry only availability
;; information: defined / undefined.  The output of the abstract
;; evaluation step is sequential code consisting of expression
;; evaluation (a constraint is turned into a directed function)
;; followed by assigment/binding.

;; Abstract domain has 2 values: defined / undefined.
(define-struct defined ())
(define-struct undefined ())

;; The function `node-assert!' will set a node's value, and prompt all
;; associated rules to compute and propagate.  As a side effect, this
;; generates sequential code that performs the same sequence of
;; operations on values available at run-time.
(define (node-assert! node value)
  (unless (undefined? (node-value node))
    (error 'redefine-node "~a with ~a" (node->string node) value))
  (set-node-value! node value) ;; assert before recurse
  (for-each rule-bang! (node-rules node))) ;; propagate

  

                  
;; Code sequencer driver:
;;   1. create network
;;   2. connect constraints
;;   3. assert inputs (this triggers propagation)
;;   4. collect sequenced code

;; Build network structure.
(define (spec->net inputs outputs internal rules params)
  (let ((net (make-node-net (append inputs outputs internal))))
    (for ((r rules) (p params)) (net-constrain! net r p))
    net))

;; Evaluate network -> sequential code.

(require srfi/41
         "enum.ss")


(define (net-eval net inputs)
  (define enum
    (solutions
     (query/parameterize
        ((code-stack '())
         (value-stack '()))
      (for ((i inputs)) (net-assert! net i))
      (code-stack))))
  (stream-car
   (enum->stream enum)))

(define (spec->sequence inputs outputs . rest-spec)
  (let ((net (apply spec->net inputs outputs rest-spec)))
    #`(lambda #,inputs
        (let* #,(reverse (net-eval net inputs))
          (values #,@outputs)))))

    
    
