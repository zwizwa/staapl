#lang racket/base

(require racket/file)

(provide with-file
         file->string/utf-8)

(define (with-file open current close make)
  (lambda (mf thunk)
    (define port #f)
    (unless mf (set! mf (make)))
    (dynamic-wind
      (lambda () (set! port (open mf)))
      (lambda () (parameterize ((current port)) (thunk)))
      (lambda () (close port)))))

(define (file->string/utf-8 file)
  (bytes->string/utf-8 (file->bytes file)))
