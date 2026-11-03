(use-modules (gnu services)
             (gnu services monitoring)
             (gnu system)
             (srfi srfi-1))

(define (load-last-expression path)
  (call-with-input-file path
    (lambda (port)
      (let loop ((last #f))
        (let ((expression (read port)))
          (if (eof-object? expression)
              last
              (loop (eval expression (current-module)))))))))

(define os (load-last-expression "guix-system/config/nagi.scm"))

(define exporter
  (find (lambda (service)
          (eq? prometheus-node-exporter-service-type (service-kind service)))
        (operating-system-services os)))

(unless exporter
  (error "nagi must expose host metrics through node_exporter"))

(unless (equal? ":9100"
                (prometheus-node-exporter-web-listen-address
                 (service-value exporter)))
  (error "node_exporter must listen on port 9100 for remote scraping"))

(unless (member "--collector.drm"
                ((@@ (gnu services monitoring) prometheus-node-exporter-extra-options)
                 (service-value exporter)))
  (error "nagi must collect AMD GPU utilization and memory metrics"))
