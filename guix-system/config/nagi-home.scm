(use-modules (gnu home)
             (gnu services)
             (gnu home services)
             (gnu home services shepherd)
             (guix gexp)
             (gnu packages rust-apps))

(home-environment
  (services
   (cons*
    (service home-shepherd-service-type
             (home-shepherd-configuration
               (services
                (list
                 (shepherd-service
                   (provision '(xremap))
                   (documentation "xremap key remapping daemon")
                   (auto-start? #t)
                   (respawn? #t)
                   (start #~(lambda ()
                              (let* ((config (string-append (getenv "HOME")
                                                            "/.config/xremap/config.yml")))
                                (make-forkexec-constructor
                                 (list #$(file-append xremap-wlroots "/bin/xremap") "--watch" config)
                                 #:environment-variables
                                 (let ((env (environ)))
                                   (define (fallback name default)
                                     (or (getenv name) default))
                                   (append
                                    env
                                    (list
                                     (string-append "WAYLAND_DISPLAY="
                                                    (fallback "WAYLAND_DISPLAY" "wayland-1")))))))))
                   (stop #~(make-kill-destructor)))))))
    %base-home-services)))
