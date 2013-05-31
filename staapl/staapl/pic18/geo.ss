#lang scheme/base
;; FIXME: this is add-on library, not core.

;; (require staapl/pic18)
(require "../macro.ss"
         "../target.ss"
         "../scat.ss")
(provide (all-defined-out))



;; Generate a geometric sequence. This is used in synth-control.f and
;; serves as an example of how to write a custom code generator in
;; scheme, and include it in a .f project.

;; (*) Note that assembly-time expressions might not be available at
;; compile time. The reason for this is that they could depend on
;; not-yet-assigned target code addresses. The form 'tv: will
;; handle these evaluations correctly, constructing an appropriate
;; assembly-time expression. However, if a numeric value is necessary
;; for code generation, it needs to be forced explicitly using
;; 'target-value->number.

;; (**) The 'list->macro function glues together constants interleaved
;; with some compilation macro, and produces a macro. This macro is
;; subsequently used as a delegate for the geo-seq invocation.

(patterns
 (macro)
 (([qw start] [qw endx] [qw n] [qw comma] compile-geo-seq)
  (let ((size (target-value->number n))) ;; (*)
    (define (element index)
      (tv: index size /
           endx start / pow
           start *))
    (list->macro ;; (**)
     comma
     (for/list ((i (in-range size))) (element i))))))

