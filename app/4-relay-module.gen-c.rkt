#lang racket/base
;; Generate C code for console
(require "4-relay-module.fm")
(require staapl/live/c-console)
(print-dictionary-struct-init (words->dict relay! diag))
(printf "#include \"../live/console.c\"\n")
