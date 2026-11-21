(use-modules (gnu home)
             (gnu home services)
             (gnu services)
             (gnu packages base)
             (nongnu packages password-utils)
             (guix gexp)
             (guix store)
             (guix derivations)
             (ice-9 textual-ports)
             (srfi srfi-13))

(define home
  (load (string-append (getcwd) "/guix-system/config/nagi-home.scm")))

(define launcher
  (assoc-ref (service-value (fold-services (home-environment-services home)
                                           #:target-type home-files-service-type))
             ".local/share/applications/bitwarden.desktop"))

(unless launcher
  (error "Guix Home must install the Bitwarden Wayland launcher"))

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

(display "PASS: Bitwarden Wayland launcher preserves upstream fields\n")
