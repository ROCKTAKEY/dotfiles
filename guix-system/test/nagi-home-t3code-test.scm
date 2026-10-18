(use-modules (gnu home)
             (gnu services)
             (gnu packages base)
             (roquix home services t3code)
             (roquix packages t3code)
             (srfi srfi-1))

;; Exercise package selection independently of the channel's service default.
(module-set! (resolve-module '(roquix home services t3code))
             'home-t3code-service-type
             (service-type
              (inherit home-t3code-service-type)
              (default-value (home-t3code-configuration (t3code hello)))))

(define home
  (load (string-append (getcwd) "/guix-system/config/nagi-home.scm")))

(define t3code-service
  (find (lambda (service)
          (eq? (service-kind service) home-t3code-service-type))
        (home-environment-services home)))

(if (and t3code-service
         (eq? t3code-cli
              (home-t3code-configuration-t3code
               (service-value t3code-service))))
    (display "PASS: T3 Code explicitly selects t3code-cli\n")
    (begin
      (display "FAIL: T3 Code must explicitly select t3code-cli\n"
               (current-error-port))
      (exit 1)))
