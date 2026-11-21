(use-modules (gnu home)
             (gnu services)
             (gnu packages base)
             (nongnu packages password-utils)
             (guix gexp)
             (guix store)
             (guix derivations)
             (ice-9 textual-ports)
             (srfi srfi-1)
             (srfi srfi-13))

(define home
  (load (string-append (getcwd) "/guix-system/config/nagi-home.scm")))

(define (find-home-service name)
  (find (lambda (service)
          (eq? (service-type-name (service-kind service)) name))
        (home-environment-services home)))

(define agent-service
  (find-home-service 'bitwarden-ssh-agent))

(unless (and agent-service
             (equal? (assoc-ref (service-value agent-service) "SSH_AUTH_SOCK")
                     "$HOME/.bitwarden-ssh-agent.sock"))
  (error "SSH clients must use the Bitwarden agent socket in the user's home"))

(define launcher-service
  (find-home-service 'bitwarden-wayland-launcher))

(unless launcher-service
  (error "Bitwarden must have a Wayland launcher"))

(define launcher
  (assoc-ref (service-value launcher-service)
             ".local/share/applications/bitwarden.desktop"))

(with-store store
  (let* ((drv (run-with-store store (lower-object (car launcher))))
         (original (run-with-store
                    store
                    (lower-object bitwarden-desktop)))
         (env (run-with-store store
                (lower-object coreutils))))
    (build-derivations store (list drv))
    (let* ((source (call-with-input-file
                    (string-append (derivation->output-path original)
                                   "/share/applications/bitwarden.desktop")
                    get-string-all))
           (actual (call-with-input-file (derivation->output-path drv)
                     get-string-all))
           (expected
            (string-join
             (map (lambda (line)
                    (if (string-prefix? "Exec=" line)
                        (string-append "Exec="
                                       (derivation->output-path env)
                                       "/bin/env DISPLAY_MODE=WAYLAND "
                                       (substring line 5))
                        line))
                  (string-split source #\newline))
             "\n")))
      (unless (string-contains source "\nExec=")
        (error "Upstream desktop entry must contain Exec"))
      (unless (string=? actual expected)
        (error "Only the launch environment may differ from upstream")))))

(display "PASS: Bitwarden SSH agent is selected; Wayland launcher preserves upstream fields\n")
