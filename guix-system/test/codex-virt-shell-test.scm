(use-modules (roquix extra-profiles shell-configuration)
             (roquix extra-profiles paths)
             (srfi srfi-1)
             (srfi srfi-64))

(setenv "XDG_RUNTIME_DIR" "/run/user/1000")
(setenv "WAYLAND_DISPLAY" "wayland-1")
(setenv "DBUS_SESSION_BUS_ADDRESS" "unix:path=/run/user/1000/bus")

(define config
  (load (string-append (getcwd)
                       "/guix/.config/guix/extra-profiles/virt/shell.scm")))

(define (shared? source)
  (any (lambda (mount)
         (and (string=? source (shell-mount-source mount))
              (eq? 'read-write (shell-mount-access mount))))
       (shell-configuration-mounts config)))

(test-begin "codex-virt-shell")
(define runner (test-runner-current))
(test-assert "desktop sockets remain available"
  (and (shared? "/run/user/1000/bus")
       (shared? "/run/user/1000/wayland-1")))
(test-assert "Codex and virt compose without runtime mount conflicts"
  (compose-shell-configurations
   (list
    (cons (parse-profile-name "codex")
          (shell-configuration
           (container? #t)
           (mounts (list (share "/tmp/codex-runtime"
                               #:target "/run/user/1000")))))
    (cons (parse-profile-name "virt") config))))
(test-end "codex-virt-shell")
(exit (if (zero? (test-runner-fail-count runner)) 0 1))
