;; What follows is a "manifest" equivalent to the command line you gave.
;; You can store it in a file that you may then pass to any 'guix' command
;; that accepts a '--manifest' (or '-m') option.
(use-modules (guix profiles))

(define here
  (let ((f (current-filename)))
    (if f (dirname f) ".")))

(define (from-here rel)
  (string-append here "/" rel))

(concatenate-manifests
 (list
  (specifications->manifest
   (list "coreutils"
         "gcc-toolchain"
         "glibc"
         "openssl"
         "node"))
  (load (from-here "codex.scm"))))
