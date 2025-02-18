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
    (start #~(make-system-constructor
              (string-join
               (list #$(file-append xremap-x11 "/bin/xremap")
                     "--watch=device,config" #$(home-xremap-configuration-config-file config)))))
    (documentation "On startup, run @code{xmodmap} and read the expressions in
the configuration file.  On stop, reset all the mappings back to the
defaults."))))

(define home-xremap-service-type
  (service-type
   (name 'home-xremap)
   (extensions
    (list
     (service-extension home-shepherd-service-type
                        xremap-shepherd-service)))
   (description "Run the @code{xmodmap} utility to modify keymaps and pointer
buttons under the Xorg display server via user-defined expressions.")))

(home-environment
  (services
   (cons*
    (service home-xremap-service-type
             (home-xremap-configuration
              (config-file (plain-file "/home/rocktakey/.config/xremap/config.yml"))))
    %base-home-services)))
