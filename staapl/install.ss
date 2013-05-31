#lang scheme/base

(require launcher/launcher
         scheme/cmdline)


(define prefix-bin (make-parameter "/usr/local/bin"))

(command-line
 #:program "staapl/install"
 #:once-each
 [("--prefix-bin") path "Where to link executables (default: /usr/local/bin)"  (prefix-bin path)]
 #:args () (void))

(define (overwrite-link src dst)
  (when (file-exists? dst)
    (delete-file dst))
  (make-file-or-directory-link src dst)
  (printf "linked: ~a -> ~a\n" dst src))

(define (install prog)
  (let ((at (mzscheme-program-launcher-path prog)))
    (make-mzscheme-launcher
     `("-p" ,(format "zwizwa/staapl/~a" prog) "--") at)
    (printf "installed: ~a\n" (path->string at))
    (with-handlers ((void void)) ;; attempt.. might fail due to permissions
      (case (system-type)
        ((unix macosx)
         (overwrite-link at (format "~a/~a" (prefix-bin) prog)))
        (else (void))))))


(install "staaplc")
