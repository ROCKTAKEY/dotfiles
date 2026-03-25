(use-modules (gnu services)
             (gnu services base)
             (gnu services web)
             (gnu system)
             (srfi srfi-1))

(define (assert label predicate)
  (if predicate
      (format #t "PASS: ~a~%" label)
      (begin
        (format (current-error-port) "FAIL: ~a~%" label)
        (exit 1))))

(define (load-last-expression path)
  (call-with-input-file path
    (lambda (port)
      (let loop ((last #f))
        (let ((expression (read port)))
          (if (eof-object? expression)
              last
              (loop (eval expression (current-module)))))))))

(define (service-of-type services type)
  (find (lambda (service)
          (eq? (service-kind service) type))
        services))

(define operating-system-configuration
  (load-last-expression "guix-system/config/cuirass.scm"))

(define services
  (operating-system-services operating-system-configuration))

(define guix-publish-service
  (service-of-type services guix-publish-service-type))

(define nginx-service
  (service-of-type services nginx-service-type))

(assert "guix-publish service exists"
        guix-publish-service)

(assert "guix-publish listens on localhost only"
        (and guix-publish-service
             (string=? "localhost"
                       (guix-publish-configuration-host
                        (service-value guix-publish-service)))))

(assert "guix-publish moves behind nginx"
        (and guix-publish-service
             (= 3000
                (guix-publish-configuration-port
                 (service-value guix-publish-service)))))

(assert "nginx service exists"
        nginx-service)

(assert "nginx proxies requests to guix-publish"
        (and nginx-service
             (any (lambda (server)
                    (and (member "80"
                                 (nginx-server-configuration-listen server))
                         (any (lambda (location)
                                (and (string=? "/"
                                               (nginx-location-configuration-uri
                                                location))
                                     (member "proxy_pass http://localhost:3000;"
                                             (nginx-location-configuration-body
                                              location))))
                              (nginx-server-configuration-locations server))))
                  (nginx-configuration-server-blocks
                   (service-value nginx-service)))))
