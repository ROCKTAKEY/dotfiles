(use-modules (gnu services)
             (gnu system)
             (guix gexp)
             (guix store)
             (guix derivations)
             (json)
             (ice-9 popen)
             (roquix packages virtiofsd)
             (srfi srfi-1))

(define operating-system-configuration
  (primitive-load "guix-system/config/nagi.scm"))

(unless (member virtiofsd
                (operating-system-packages operating-system-configuration))
  (error "nagi must install virtiofsd in the system profile"))

(define virtiofsd-service
  (find (lambda (service)
          (eq? 'virtiofsd-vhost-user
               (service-type-name (service-kind service))))
        (operating-system-services operating-system-configuration)))

(unless virtiofsd-service
  (error "nagi must register virtiofsd for libvirt's vhost-user discovery"))

(define descriptor
  (assoc "qemu/vhost-user/50-virtiofsd.json"
         (service-value virtiofsd-service)))

(unless descriptor
  (error "nagi must install virtiofsd's vhost-user descriptor under /etc"))

(with-store store
  (let ((drv (run-with-store store (lower-object (cadr descriptor)))))
    (build-derivations store (list drv))
    (let* ((fields (call-with-input-file (derivation->output-path drv) json->scm))
           (binary (assoc-ref fields "binary")))
      (unless (and (equal? "fs" (assoc-ref fields "type"))
                   (string? binary)
                   (string-prefix? "/" binary)
                   (access? binary X_OK))
        (error "Invalid filesystem backend descriptor" fields))
      (let* ((port (open-pipe* OPEN_READ binary "--print-capabilities"))
             (capabilities (json->scm port)))
        (unless (and (zero? (close-pipe port))
                     (equal? "fs" (assoc-ref capabilities "type")))
          (error "The configured backend must advertise filesystem support"))))))
