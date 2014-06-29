#lang racket/base

;; mfile.ss

;; Managed files and directories.  Managed files are scheme objects
;; with a 1-1 correspondence to filesystem objects.  Managed
;; directories are simply hash tables mapping path names to managed
;; files.

;; Basicly this abstraction allows one to treat unix file(s) ->
;; file(s) processing programs as Scheme functions, with automatic
;; garbage collection of intermediate results.  See latex.ss for an
;; example.

(provide make-mfile
         open-output-mfile
         open-input-mfile
         with-output-to-mfile
         mfile-size

         mfile->bytes
         bytes->mfile
         
         mfile->string/utf-8
         string->mfile/utf-8

         make-mdir
         mdir-refs
         with-mdir
         mdir-sorted

         )

(require "file.rkt"
         scheme/foreign
         racket/file)




;; Identifiers
(define *id* 0)
(define *id-semaphore* (make-semaphore 1))
(define (make-id)
  (semaphore-wait *id-semaphore*)
  (let ((id *id*))
    (set! *id* (add1 *id*))
    (semaphore-post *id-semaphore*)
    id))

(define (make-temp-path)
  (build-path (temp-dir) (format "~s" (make-id))))

(define *temp-dir*
  ;; Try to get dir from environment.  This allows a script to start
  ;; mzscheme and cleanup the tempdir afterwards.
  (let ((TMPDIR (getenv "TMPDIR")))
    (if TMPDIR
        (let ((dir (string->path TMPDIR)))
          (printf "mfile directory: ~a\n" TMPDIR)
          dir)
        (make-temporary-file "mz~a" 'directory))))

(define (temp-dir) *temp-dir*)   


;; Scheme object
(define-struct mfile-meta (path))

(define (delete path)
  (define (warning _)
    (printf "WARNING: can't delete ~s\n" path))
  ;; (printf "deleting ~s\n" path)
  (with-handlers ((void warning))
    (delete-file path)))

(define (make-registered-mfile path)
  (define (cleanup tf) (delete (mfile-meta-path tf)))
  (define tf (make-mfile-meta path))
  (register-finalizer tf cleanup)
  tf)

(define (make-mfile [external-path #f])
  (let* ((path (make-temp-path))
         (tf (make-registered-mfile path)))
    (when external-path
      (unless (file-exists? external-path)
        (error 'mfile-file-not-found "~s" external-path))
      (make-file-or-directory-link
       (path->complete-path external-path) path))
    tf))

;; Wrap a file as a mfile, moving it into the temp dir.
(define (make-mfile-rename [original #f])
  (let ((path (make-temp-path)))
    (when original
      (rename-file-or-directory original path))
    (make-registered-mfile path)))

;; File open

(define (open-input-mfile tf
                          #:mode (m 'binary))
  (open-input-file (mfile-meta-path tf) #:mode m))
  
(define (open-output-mfile tf
                           #:mode (m 'binary)
                           #:exists (e 'error))
  (let ((path (mfile-meta-path tf)))
    (open-output-file path #:mode m #:exists e)))




;; External tools

;; Invoke a "directory processor", passing it mfiles named as
;; requested.  A directory is represented as a hash table mapping
;; names to mfiles.  They will be hard-linked into the temporary
;; directory, so it is possible to modify objects in-place.

(define (with-mdir bindings thunk)
  (define tempdir (make-temp-path))
  (define output (make-hash))
  (define (file filename) (build-path tempdir filename))

  (unless (hash? bindings)
    (set! bindings (make-immutable-hash bindings)))
  
  (dynamic-wind
    ;; Link input objects into temp dir with the requested name.
    (lambda ()
      (make-directory tempdir)
      (for (((name tf) bindings))
        (make-file-or-directory-link
         (mfile-meta-path tf) (file name))))

    ;; Perform computation.
    (lambda ()
      (parameterize ((current-directory tempdir))
        (thunk) output))

    ;; Wrap the newly created output files as mfiles.
    (lambda ()
      (for (((filename _) bindings))
        (delete (file filename)))
      (for ((name (directory-list tempdir)))
        (hash-set! output name
                   (make-mfile-rename
                    (file name))))
      (delete-directory tempdir))))


(define (mdir-refs/list mdir . refs)
  (for/list ((r refs)) (hash-ref mdir (build-path r) (lambda _ #f))))
(define (mdir-refs . args)
  (apply values (apply mdir-refs/list args)))



(define make-mdir make-hash)


(define with-output-to-mfile
  (with-file open-output-mfile
             current-output-port
             close-output-port
             make-mfile))
(define with-input-from-mfile
  (with-file open-input-mfile
             current-input-port
             close-input-port
             make-mfile))

(define (mfile-size mfile)
  (file-size (mfile-meta-path mfile)))

(define (bytes->mfile bytes)
  (let ((mfile (make-mfile)))
    (with-output-to-mfile mfile (lambda () (write-bytes bytes)))
    mfile))

(define (mfile->bytes mfile)
  (with-input-from-mfile
   mfile
   (lambda () (read-bytes (mfile-size mfile)))))

     
(define (mfile->string/utf-8 mfile)
  (bytes->string/utf-8 (mfile->bytes mfile)))
(define (string->mfile/utf-8 str)
  (bytes->mfile (string->bytes/utf-8 str)))

(define (mdir-sorted mdir)
  (map cdr
       (sort
        (for/list (((name mf) mdir))
          (cons (path->string name) mf))
        string-ci<? #:key car)))
  
