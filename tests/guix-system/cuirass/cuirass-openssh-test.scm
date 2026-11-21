(use-modules (gnu services)
             (gnu services shepherd)
             (gnu services ssh)
             (gnu system)
             (guix derivations)
             (guix gexp)
             (guix store)
             (guix utils)
             (ice-9 popen)
             (ice-9 textual-ports)
             (srfi srfi-1))

(define operating-system-configuration
  (primitive-load "guix-system/config/cuirass.scm"))

(define services
  (operating-system-services operating-system-configuration))

(define openssh-service
  (find (lambda (service)
          (openssh-configuration? (service-value service)))
        services))

(unless openssh-service
  (error "Cuirass must provide an OpenSSH service"))

(define config (service-value openssh-service))

(with-store store
  (define (build-file object)
    (let ((drv (run-with-store store (lower-object object))))
      (build-derivations store (list drv))
      (derivation->output-path drv)))
  (let ((openssh (build-file (openssh-configuration-openssh config)))
        (file (build-file ((@@ (gnu services ssh) openssh-config-file) config))))
    (call-with-temporary-directory
     (lambda (directory)
       (define key (string-append directory "/host-key"))
       (define (check-policy peer local refuse?)
         (let* ((connection (string-append "user=root,addr=" peer ",laddr=" local))
                (port (open-pipe* OPEN_READ (string-append openssh "/sbin/sshd")
                                  "-T" "-h" key "-f" file "-C" connection))
                (settings (string-split (string-downcase (get-string-all port)) #\newline)))
           (unless (zero? (close-pipe port))
             (error "sshd rejected its configuration" connection))
           (for-each
            (lambda (setting)
              (unless (member setting settings)
                (error "Unexpected effective SSH policy" connection setting)))
            (cons (if refuse? "refuseconnection yes" "refuseconnection no")
                  '("pubkeyauthentication yes" "permitrootlogin prohibit-password"
                    "passwordauthentication no" "kbdinteractiveauthentication no")))))
       (unless (zero? (system* (string-append openssh "/bin/ssh-keygen")
                               "-q" "-t" "ed25519" "-N" "" "-f" key))
         (error "Could not create a temporary host key"))
       (check-policy "100.64.0.1" "100.64.0.2" #f)
       (check-policy "192.0.2.1" "100.64.0.2" #t)
       (check-policy "100.64.0.1" "192.0.2.2" #t)
       (check-policy "192.0.2.1" "192.0.2.2" #t)))))

(unless (any (lambda (service)
               (eq? 'cuirass-ssh-firewall
                    (service-type-name (service-kind service))))
             services)
  (error "Cuirass must restrict SSH connections with a dedicated firewall"))

(define shepherd-extension
  (find (lambda (extension)
          (eq? shepherd-root-service-type
               (service-extension-target extension)))
        (service-type-extensions (service-kind openssh-service))))

(define ssh-daemon
  (find (lambda (service)
          (memq 'ssh-daemon (shepherd-service-provision service)))
        ((service-extension-compute shepherd-extension) config)))

(unless (and ssh-daemon
             (memq 'cuirass-ssh-firewall
                   (shepherd-service-requirement ssh-daemon)))
  (error "The SSH listener must wait for the Tailscale firewall"))
