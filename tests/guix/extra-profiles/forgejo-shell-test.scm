(use-modules (guix utils)
             (roquix extra-profiles paths)
             (roquix extra-profiles shell-configuration)
             (roquix extra-profiles shell-mounts)
             (srfi srfi-1)
             (srfi srfi-64))

(define definition-directory
  (string-append (getcwd) "/guix/.config/guix/extra-profiles"))

(test-begin "forgejo-shell")
(define runner (test-runner-current))
(for-each
 (lambda (name)
   (test-assert (string-append name " prepares writable Forgejo state on first use")
     (call-with-temporary-directory
      (lambda (home)
        (let* ((paths (map (lambda (suffix) (string-append home "/" suffix))
                           '(".config/forgejo-cli" ".local/share/forgejo-cli" ".cache/git")))
               (profile (with-environment-variables `(("HOME" ,home))
                          (load (string-append definition-directory "/" name "/shell.scm"))))
               ;; Other mounts require a real user's keys and desktop session.
               (mounts (filter (lambda (mount) (member (shell-mount-target mount) paths))
                               (shell-configuration-mounts profile)))
               (plan (compose-shell-mounts
                      (list (cons (parse-profile-name name)
                                  (shell-configuration (mounts mounts))))))
               (arguments (prepared-shell-mount-arguments (prepare-shell-mounts plan))))
          (and (zero? (apply system* "guix" "shell" "-C" "--no-cwd"
                             (append arguments
                                     '("--" "/bin/sh" "-ec"
                                       "for path do echo ok > \"$path/probe\"; done" "sh")
                                     paths)))
               (every (lambda (path) (file-exists? (string-append path "/probe")))
                      paths)))))))
 '("codex" "opencode"))
(test-end "forgejo-shell")
(exit (if (zero? (test-runner-fail-count runner)) 0 1))
