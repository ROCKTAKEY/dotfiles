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

(define configurations
  (map service-value
       (filter (lambda (service)
                 (eq? (service-kind service) home-t3code-service-type))
               (home-environment-services home))))

(unless (= 2 (length configurations))
  (error "Home must run the primary and local LLM T3 Code instances"))

(define (configuration-named name)
  (find (lambda (config)
          (eq? name (home-t3code-configuration-service-name config)))
        configurations))

(define primary (configuration-named 't3code))
(define local-llm (configuration-named 't3code-local))

(unless (and primary local-llm
             (every (lambda (config)
                      (eq? t3code-cli-rocktakey
                           (home-t3code-configuration-t3code config)))
                    configurations)
             (= 3773 (home-t3code-configuration-port primary))
             (not (home-t3code-configuration-data-directory primary))
             (= 3774 (home-t3code-configuration-port local-llm))
             (equal? "/home/rocktakey/.t3-local"
                     (home-t3code-configuration-data-directory local-llm))
             (every (lambda (config)
                      (string=? "127.0.0.1"
                                (home-t3code-configuration-host config)))
                    configurations))
  (error "T3 Code instances must use the fork and independent local endpoints and state"))

(display "PASS: primary and local LLM T3 Code instances are independently configured\n")
