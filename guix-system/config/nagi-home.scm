(use-modules (gnu home)
             (gnu home services)
             (gnu home services desktop)
             (gnu home services shepherd)
             (gnu services)
             (gnu services configuration)
             (gnu packages admin)
             (guix gexp)
             (gnu packages rust-apps))


(define-configuration/no-serialization home-xremap-configuration
  (config-file
   (file-like)
   "Path to config.yml file for xremap."))

(define (xremap-shepherd-service config)
  (list
   (shepherd-service
    (provision '(xremap))
    (modules '((shepherd support)))
    (start #~(make-forkexec-constructor
              (list #$(file-append xremap-x11 "/bin/xremap")
                    "--watch=device"
                    #$(home-xremap-configuration-config-file config))
              #:log-file (string-append %user-log-dir "/xremap.log")))
    (stop #~(make-kill-destructor))
    (documentation "Run @code{xremap} with the configuration file."))))

(define home-xremap-service-type
  (service-type
   (name 'home-xremap)
   (extensions
    (list
     (service-extension home-shepherd-service-type
                        xremap-shepherd-service)))
   (description "Run the @code{xremap} utility to modify keymaps.")))

(home-environment
  (services
   (cons*
    (service home-xremap-service-type
             (home-xremap-configuration
              (config-file (local-file "/home/rocktakey/.config/xremap/config.yml"))))
    %base-home-services)))
