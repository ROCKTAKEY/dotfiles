(use-modules (gnu services)
             (gnu services shepherd)
             (gnu services ssh)
             (gnu system)
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

(unless (openssh-configuration-public-key-authentication? config)
  (error "Cuirass must allow public key authentication"))

(unless (eq? 'prohibit-password
             (openssh-configuration-permit-root-login config))
  (error "Cuirass must allow root login without password authentication"))

(when (openssh-configuration-password-authentication? config)
  (error "Cuirass must disable password authentication for all users"))

(when (openssh-configuration-challenge-response-authentication? config)
  (error "Cuirass must disable keyboard-interactive authentication for all users"))

(unless (string-null? (openssh-configuration-extra-content config))
  (error "Cuirass must not restrict SSH connections with extra match rules"))

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
