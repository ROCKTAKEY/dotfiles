(use-modules (gnu services)
             (gnu services ssh)
             (gnu system)
             (srfi srfi-1))

(define operating-system-configuration
  (primitive-load "guix-system/config/cuirass.scm"))

(define services
  (operating-system-services operating-system-configuration))

(define openssh-service
  (find (lambda (service)
          (eq? openssh-service-type (service-kind service)))
        services))

(unless openssh-service
  (error "Cuirass must use the standard OpenSSH service"))

(define config (service-value openssh-service))

(unless (openssh-configuration-public-key-authentication? config)
  (error "Cuirass must allow public key authentication"))

(unless (eq? 'prohibit-password
             (openssh-configuration-permit-root-login config))
  (error "Cuirass must allow root login without password authentication"))

(unless (openssh-configuration-password-authentication? config)
  (error "Cuirass must retain default password authentication for other users"))

(unless (string-null? (openssh-configuration-extra-content config))
  (error "Cuirass must not restrict SSH connections with extra match rules"))

(when (any (lambda (service)
             (eq? 'cuirass-ssh-firewall
                  (service-type-name (service-kind service))))
           services)
  (error "Cuirass must not restrict SSH connections with a dedicated firewall"))
