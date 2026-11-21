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

(define (state-directory config)
  ;; For an unset data-directory the service passes $HOME/.t3 as --base-dir.
  (or (home-t3code-configuration-data-directory config) "/home/rocktakey/.t3"))

(unless (and primary local-llm
             (not (= (home-t3code-configuration-port primary)
                     (home-t3code-configuration-port local-llm)))
             (not (string=? (state-directory primary) (state-directory local-llm))))
  (error "Primary and local T3 Code instances must have independent ports and state"))

(unless (every (lambda (config)
                 (and (eq? t3code-cli-rocktakey (home-t3code-configuration-t3code config))
                      (string=? "127.0.0.1" (home-t3code-configuration-host config))))
               configurations)
  (error "Both T3 Code instances must use the fork and listen on loopback"))
