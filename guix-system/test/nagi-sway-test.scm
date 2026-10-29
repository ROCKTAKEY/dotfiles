(use-modules (gnu system))

(define operating-system-configuration
  (call-with-input-file "guix-system/config/nagi.scm"
    (lambda (port)
      (let loop ((last #f))
        (let ((expression (read port)))
          (if (eof-object? expression)
              last
              (loop (eval expression (current-module)))))))))

(unless (memq (@ (roquix packages sway) sway)
              (operating-system-packages operating-system-configuration))
  (error "nagi must install roquix sway with the json-c memory leak fix"))
