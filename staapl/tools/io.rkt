#lang racket/base

(require (lib "pretty.rkt")
         (lib "match.rkt")
         (lib "process.rkt") ;; system
         )
(provide
 read-byte-timeout
 with-output-to-file/safe
 with-output-to-string
 write-tree
 file-in-path
 resolve-path-list
 filename->path
 file->syntax-list
 port->syntax-list
 when-file
 )


;; GENERIC PORT IO

;; TODO: abstract this in a 'lazy-open' library.

;; False if timeout. Input ports are synchronizable events in plt
;; scheme. This predicate guarantees read-byte will not block.
(define (port-ready? timeout port)
  (sync/timeout timeout port))


;; Read a byte from port with timeout in seconds.
(define (read-byte-timeout port timeout)
  (let again ()
    (if (port-ready? timeout port)
        (read-byte port)
        (begin
          (error 'time-out "~a" timeout)
          ))))

;; Darcs-friendly saving of data file. Using the pretty printer
;; seems to be friendly enough.
(define (write-tree . args)
  (apply pretty-print args))


;; Lookup a file in a search path.
(define (file-in-path path filename)
  (let next ((p path))
    (if (null? p)
        (error 'file-not-in-path "~a ~a" filename path)
        (let ((full (format "~a/~a" (car p) filename)))
          (if (file-exists? full)
              full
              (next (cdr p)))))))

;; Remove a file if it exists.
(define (delete-if-exists file)
  (when (file-exists? file) (delete-file file)))

;; Save to a file, but do it safely.  
(define (with-output-to-file/safe file thunk)
  (define (add-suffix f s)
    (string-append (path->string (build-path f)) s))
  
  (let ((file.bak (add-suffix file "~"))
        (file.tmp (add-suffix file ".bak")))

    ;; In case thunk fails, first write to a temp file.
    (delete-if-exists    file.tmp)
    (let ((value
           (with-output-to-file
               file.tmp thunk)))

      ;; Cycle backups.
      (delete-if-exists    file.bak)
      (when (file-exists? file)
        (rename-file-or-directory file file.bak))
      (rename-file-or-directory file.tmp file)

      value)))


(define (with-output-to-string thunk)
  (let ((p (open-output-string)))
    (parameterize ((current-output-port p)) (thunk))
    (close-output-port p)
    (get-output-string p)))


;; By default use the abs-file path (which is constructed using
;; load-relative). If that doesn't exist, find the rel-file in the
;; search path.
(define (resolve-path-list rel-file path-list)
  (cond
   ((file-exists? rel-file) rel-file)
   ((absolute-path? rel-file) (error 'not-found "~a" rel-file))
   (else
    (let next ((lst path-list))
      (if (null? lst)
          (error 'file-not-found "~a" rel-file)
          (let ((file (build-path (car lst) rel-file)))
            ;; (printf "path: ~a\n" file)
            (if (file-exists? file) file
                (next (cdr lst)))))))))


(define (port->syntax-list [port (current-input-port)] [stx #f])
  (let slurp ()
    (let ((atom (read port)))
      (if (eof-object? atom) '()
          (cons (datum->syntax stx atom) (slurp))))))
              
(define (file->syntax-list filename [stx #f])
  (port->syntax-list (open-input-file filename) stx))
              
(define (filename->path filename)
  (let-values (((path _ __) (split-path filename)))
    (when (eq? path 'relative)    (set! path (current-load-relative-directory)))
    (unless (complete-path? path) (set! path (path->complete-path path)))
    ;; (printf "P: ~a\n" path)
    path))


(define (when-file f fn)
  (when (file-exists? f)
    (fn f)))
