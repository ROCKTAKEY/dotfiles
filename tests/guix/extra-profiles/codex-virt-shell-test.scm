(use-modules (roquix extra-profiles shell-configuration)
             (roquix extra-profiles paths)
             (srfi srfi-1)
             (srfi srfi-64))

(define runtime (string-append "/run/user/" (number->string (getuid))))
(setenv "XDG_RUNTIME_DIR" runtime)
(setenv "WAYLAND_DISPLAY" "wayland-1")
(setenv "DBUS_SESSION_BUS_ADDRESS" (string-append "unix:path=" runtime "/bus"))

(test-begin "codex-virt-shell")
(define runner (test-runner-current))
(test-assert "Desktop sockets are shared inside the combined Codex and virt runtime"
  (let* ((combined
          (compose-shell-configurations
           (map (lambda (name)
                  (cons (parse-profile-name name)
                        (load (string-append (getcwd) "/guix/.config/guix/extra-profiles/"
                                             name "/shell.scm"))))
                '("codex" "virt"))))
         (mounts (map requested-shell-mount-value
                      (shell-mount-plan-requests
                       (composed-shell-configuration-mounts combined)))))
    (every (lambda (name)
             (let ((path (string-append runtime "/" name)))
               (any (lambda (mount)
                      (and (string=? path (shell-mount-source mount))
                           (string=? path (shell-mount-target mount))
                           (eq? 'read-write (shell-mount-access mount))))
                    mounts)))
           '("bus" "wayland-1"))))
(test-end "codex-virt-shell")
(exit (if (zero? (test-runner-fail-count runner)) 0 1))
