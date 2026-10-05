(use-modules (gnu home)
             (gnu services)
             (gnu home services)
             (gnu home services desktop)
             (gnu home services fontutils)
             (gnu home services shepherd)
             (gnu home services sound)
             (guix gexp)
             (gnu packages fonts)
             (gnu packages rust-apps)
             (gnu packages sync)
             (gnu packages xdisorg)
             (roquix home services t3code))

(define nextcloud-autostart
  (simple-service
   'nextcloud-autostart
   home-xdg-configuration-files-service-type
   ;; Match Nextcloud's own autostart filename so both settings share one entry.
   ;; https://github.com/nextcloud/desktop/blob/master/src/common/utility_unix.cpp
   `(("autostart/com.nextcloud.desktopclient.nextcloud.desktop"
      ,(mixed-text-file
        "nextcloud.desktop"
        "[Desktop Entry]\n"
        "Type=Application\n"
        "Name=Nextcloud\n"
        "Exec=" (file-append nextcloud-client "/bin/nextcloud") " --background\n"
        "Terminal=false\n")))))

(define monospace-fonts
  (simple-service 'monospace-fonts home-fontconfig-service-type
                  (map (lambda (family)
                         `(alias (family ,family)
                                 (prefer (family "Cica"))))
                       '("monospace" "system-monospace"))))

(home-environment
  (packages (list dex font-cica))
  (services
   (cons*
    (service home-t3code-service-type)
    nextcloud-autostart
    monospace-fonts
    (service home-dbus-service-type)
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
    (service home-pipewire-service-type)
    %base-home-services)))
