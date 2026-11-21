(use-modules (guix describe)
             (guix utils)
             (roquix extra-profiles shell-mounts)
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
(test-assert "Codex SSH files can be read but cannot be changed inside a container"
  (call-with-temporary-directory
   (lambda (fixture)
     (let* ((ssh (string-append fixture "/.ssh/"))
            (paths (map (lambda (name) (string-append ssh name))
                        '("known_hosts" "ai" "ai.pub")))
            (config (with-environment-variables `(("HOME" ,fixture))
                      (load (string-append definition-directory "/codex/shell.scm"))))
            (mounts (filter (lambda (mount) (member (shell-mount-target mount) paths))
                            (shell-configuration-mounts config)))
            (plan (compose-shell-mounts
                   (list (cons (parse-profile-name "codex")
                               (shell-configuration (mounts mounts)))))))
       (mkdir ssh)
       (for-each (lambda (path)
                   (call-with-output-file path
                     (lambda (port) (display (basename path) port) (newline port))))
                 paths)
       (zero? (apply system* "guix" "shell" "-C" "--no-cwd"
                     (append (prepared-shell-mount-arguments (prepare-shell-mounts plan))
                             (list "--" "/bin/sh" "-ec"
                                   "for path do
                                      read -r value < \"$path\"
                                      test \"$value\" = \"${path##*/}\"
                                      if (echo changed >> \"$path\") 2>/dev/null; then exit 1; fi
                                    done" "sh")
                             paths)))))))
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
