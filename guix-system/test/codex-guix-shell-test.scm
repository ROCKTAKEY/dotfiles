(use-modules (guix describe)
             (roquix extra-profiles shell-configuration)
             (roquix extra-profiles paths)
             (srfi srfi-1)
             (srfi srfi-13)
             (srfi srfi-64))

(define definition-directory
  (string-append (getcwd) "/guix/.config/guix/extra-profiles"))
(define home (getenv "HOME"))

(define (host-state-shared? source mounts)
  (any (lambda (mount)
         (and (string=? source (shell-mount-source mount))
              (string=? source (shell-mount-target mount))
              (eq? 'read-write (shell-mount-access mount))))
       mounts))

(test-begin "codex-guix-shell")
(define runner (test-runner-current))
(test-assert "Nested Guix discovers host channel packages and extensions"
  (let* ((config (load (string-append definition-directory "/guix/shell.scm")))
         (variables (shell-configuration-environment-variables config))
         (modules (string-append home "/.config/guix/current/share/guile/site/"
                                 (effective-version))))
    (and (equal? modules (assoc-ref variables "GUIX_EXTENSIONS_PATH"))
         (equal? (package-path-entries)
                 (string-split (assoc-ref variables "GUIX_PACKAGE_PATH") #\:)))))
(test-assert "Guix uses the host configuration and durable profile state"
  (let* ((config (load (string-append definition-directory "/guix/shell.scm")))
         (mounts (shell-configuration-mounts config)))
    (and (shell-configuration-nesting? config)
         (every
          (lambda (source) (host-state-shared? source mounts))
          (list (string-append home "/.config/guix")
                (string-append home "/.cache/guix")
                (string-append home "/.guix-extra-profiles")
                (string-append "/var/guix/profiles/per-user/"
                               (passwd:name (getpwuid (getuid)))))))))
(test-assert "Codex and Guix shell configurations compose"
  (compose-shell-configurations
   (map (lambda (name)
          (cons (parse-profile-name name)
                (load (string-append definition-directory "/" name "/shell.scm"))))
        '("codex" "guix"))))
(test-end "codex-guix-shell")
(exit (if (zero? (test-runner-fail-count runner)) 0 1))
