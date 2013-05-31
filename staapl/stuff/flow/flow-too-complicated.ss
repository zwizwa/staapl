#lang scheme/base

;; Concatentative syntax mapped to data flow graphs:

;; (... [N1] [N2] div/mod) -> (... [N3] [N4])


;;    [N1] [N2]
;;       | |
;;    (div/mod)
;;       | |
;;    [N3] [N4]


;; This could be represented as: 

;;   [N1] = (div/mod [N1] [N2])
;;   [N2] = (shift (div/mod [N1] [N2]))

;; with dat structure sharing. this completely avoids the problem of
;; having to name intermediates.  

;; A more symmetric rep would be

;;   [N1] = ((div/mod [N1] [N2]) 0)
;;   [N2] = ((div/mod [N1] [N2]) 1)

;; which can be represented in straight scheme in the form of memoized
;; procedures.

;; Some important points:

;;   * Dataflow macros have a different representation. They have an
;;     entirely different compilation mechanism: one which involves
;;     register allocation and instruction scheduling. This
;;     representation should be made solid.

;;   * Give the dataflow macro rep, writing an automatic convertor to
;;     concatenative syntax is trivial.

;; As a result, the macro/pattern.ss mechanism is only needed as a
;; building block, not as a ui (programmer interface) front end.

;; As in any language, two things need to be defined: a composition
;; mechanism which assembles a collection of nodes into a new
;; abstracted node, and a format for defining primitive nodes.
;; The primitives I have in mind are: C code, Pure Data objects and
;; some single assignment language + register allocator for PIC.

;; So, let's concentrate on the composition. This is separated in two
;; parts: building the graph, and 'executing' it to produce the
;; desired serialized compilation target. In a first pass, i'd like to
;; build the graph only unidirectional. This allows for automatic
;; elimination of nodes whenever their output is not used. Then when
;; it's done, it can be linked bidirectionally.

;; A remaining question is where to solve the output indexing.
;; Eventually outputs are going to be addressed individually, but this
;; does require two distinct data items: subgraphs and outputs. In
;; other words, does a processing node refer to an output of another
;; processing node as a separate objeect, or does it refer directly to
;; the processing node, indexing its output namespace. When
;; flattening/inlining graph composition, the abstraction nodes
;; disappear, but the signal nodes will be part of the resulting
;; graph. This leads me to believe that separate signal nodes are a
;; good idea.

;; So, a processing 'node' is actually a directed acyclic subgraph
;; (subDAG) which connects signal nodes. Inside such a subgraph,
;; signal node fan in/out can change. This is easily solved by
;; mirroring the nodes.

;; For example in:
;;
;;     (i0)   (i1)
;;  /---|------|----\ subgraph
;;  |  (i1')  (i2') |
;;  |               |
;;  |  (o0')  (o1') |
;;  \---|------|----/
;;     (o0)   (o1)
;;
;; The connections inside the subgraph can be abstracted. This
;; over-estimates the connectivity -- all outputs depend on all inputs
;; -- but makes run time linking simpler, however
;; suboptimal. Performing static analysis (inlining a subgraph) can
;; expose the internal dependencies directly + it can eliminate
;; resources necessary to perform the linking (i.e. the edge between
;; i0 and i0' can be eliminated, connecting i0 to internal nodes).

;; Instead of representing the tree as a fully 2-way linked graph, I'm
;; going to represent it as an expression.

;; A node is either an input node (root node), in which case it does
;; not have any incoming edges, an output node in which case it does
;; not have any outgoing edges, or an intermediate node, in which case
;; it has both. In the first (functional) phase of graph construction,
;; only the incoming nodes 

(define-struct node (incoming outgoing))

;; An abstraction is a subgraph with (a namespace of) incoming nodes,
;; and (a namespace of) outgoing nodes. Maybe It's a good idea to
;; leave the naming mechanism abstract? I.e. symbols or indices?

;; A primitive is an abstract subgraph.
;; A composite subgraph is one that links together more subgraphs.

(define-struct subgraph (nodes-in nodes-out))
(define-struct (primitive subgraph) (code))
(define-struct (composite subgraph) ...)

  