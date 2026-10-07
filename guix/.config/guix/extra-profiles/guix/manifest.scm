(use-modules (guix profiles))

;; Retain the host's channel modules so nested Guix commands include extensions.
(profile-manifest (string-append (getenv "HOME") "/.config/guix/current"))
