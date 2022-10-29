;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules (gnu)
             (gnu services syncthing)
             (nongnu packages linux)
             (nongnu system linux-initrd)
             (srfi srfi-1))

(use-service-modules desktop networking ssh xorg docker linux)

(use-package-modules xorg wm certs
                     display-managers
                     freedesktop ;; libinput
                     suckless ;; st
                     )
(use-modules (nvidiachannel nvidia))
(use-modules (nvidiachannel fixpkg))

;; special xorg config.
;; adds nvidia xorg module and transforms xorg-server package
(define my-xorg-conf
  (xorg-configuration
   (modules
    (cons*
     (nfixpkg nvidia-libs-minimal)
     ;; optional: remove garbage.
     (remove
      (lambda (pkg)
        (member pkg
                (list
                 xf86-video-amdgpu
                 xf86-video-ati
                 xf86-video-cirrus
                 xf86-video-intel
                 xf86-video-mach64
                 xf86-video-nouveau
                 xf86-video-nv
                 xf86-video-sis)))
      %default-xorg-modules)))
   (server (nfixpkg xorg-server))
   (drivers '("nvidia"))))

(operating-system
 (kernel (nfixpkg linux))
 (kernel-loadable-modules (list (nfixpkg nvidia-module)))
 (kernel-arguments (list
                    ;; enable a feature
                    "nvidia-drm.modeset=1"
                    ;; nvidia_uvm gives me problems
                    "modprobe.blacklist=nouveau,nvidia_uvm"))
 (initrd microcode-initrd)
 (firmware (nfixpkgs (list linux-firmware)))

 (locale "ja_JP.utf8")
 (timezone "Asia/Tokyo")
 (keyboard-layout
  (keyboard-layout
   "jp"
   #:options
   '("ctrl:nocaps")))
 (host-name "guix-desktop")
 (users (cons* (user-account
                (name "rocktakey")
                (comment "ROCKTAKEY")
                (group "users")
                (home-directory "/home/rocktakey")
                (supplementary-groups
                 '("wheel" "netdev" "audio" "video" "docker")))
               %base-user-accounts))
 (packages
  (append
   (list (specification->package "emacs")
         (specification->package "emacs-exwm")
         (specification->package
          "emacs-desktop-environment")
         (specification->package "nss-certs")
         (specification->package "fontconfig")
         (specification->package "font-google-noto")

         ;; does this actually have to be global? i don't know. i do it anyway.
         nvidia-libs-minimal

         ;; are these even needed? i don't remember.
         xf86-input-libinput
         libinput

         ;; packages so you can actually test to see if it works
         i3-gaps ;; window manager
         st ;; terminal
         )
   %base-packages))
 (services
  (append
   (list (simple-service
          'my-nvidia-udev-rules udev-service-type
          (list (nfixpkg nvidia-udev)))
         ;; add liglvnd slim using special xorg config
         (service slim-service-type
                  (slim-configuration
                   (slim (nfixpkg slim))
                   (xorg-configuration my-xorg-conf)))

         (service kernel-module-loader-service-type
                  '("nvidia"
                    "nvidia_modeset"
                    ;; i dont remember why i put this one here.
                    ;; i think i stole it from somebody else.
                    ;; maybe it's not needed.
                    "ipmi_devintf"))


         (service gnome-desktop-service-type)
         (service syncthing-service-type
                  (syncthing-configuration (user "rocktakey")))
         ;; (set-xorg-configuration
         ;;  (xorg-configuration
         ;;   (keyboard-layout keyboard-layout)))
         (service docker-service-type))
   (remove (lambda (service)
             (or (member (service-kind service)
                         (list
                          gdm-service-type
                          modem-manager-service-type
                          usb-modeswitch-service-type
                          geoclue-service-type
                          colord-service-type))
                 (member
                  (struct-ref (service-kind service) 0)
                  '(
                    screen-locker
                    mount-setuid-helpers
                    network-manager-applet
                    ))))


           (modify-services %desktop-services
                            (guix-service-type config => (guix-configuration
                                                          (inherit config)
                                                          (substitute-urls
                                                           (append (list "https://substitutes.nonguix.org")
                                                                   %default-substitute-urls))
                                                          (authorized-keys
                                                           (append (list (local-file "../signing-keys/signing-key.pub"))
                                                                   %default-authorized-guix-keys))))))))
 (bootloader
  (bootloader-configuration
   (bootloader grub-bootloader)
   (target "/dev/sdc")
   (keyboard-layout keyboard-layout)))
 (mapped-devices
  (list (mapped-device
         (source
          (uuid "bde97474-3887-4fd9-a951-38b6233ab185"))
         (target "cryptroot")
         (type luks-device-mapping))))
 (file-systems
  (cons* (file-system
          (mount-point "/")
          (device "/dev/mapper/cryptroot")
          (type "ext4")
          (dependencies mapped-devices))
         %base-file-systems)))
