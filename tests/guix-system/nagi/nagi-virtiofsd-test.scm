(use-modules (gnu services)
             (gnu system)
             (guix gexp)
             (roquix packages virtiofsd)
             (srfi srfi-1))

(define (load-last-expression path)
  (call-with-input-file path
    (lambda (port)
      (let loop ((last #f))
        (let ((expression (read port)))
          (if (eof-object? expression)
              last
              (loop (eval expression (current-module)))))))))

(define operating-system-configuration
  (load-last-expression "guix-system/config/nagi.scm"))

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

(unless (computed-file? (cadr descriptor))
  (error "nagi must derive the descriptor from the virtiofsd package"))
