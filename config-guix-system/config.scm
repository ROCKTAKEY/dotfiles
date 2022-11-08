;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules (gnu)
             (gnu services syncthing)
             (nongnu packages linux)
             (nongnu system linux-initrd))
(use-service-modules desktop networking ssh xorg docker)

(operating-system
 (kernel linux)
 (initrd microcode-initrd)
 (firmware (list linux-firmware))

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
         (specification->package "nss-certs"))
   %base-packages))
 (services
  (append
   (list (service gnome-desktop-service-type)
         (service syncthing-service-type
                  (syncthing-configuration (user "rocktakey")))
         (set-xorg-configuration
          (xorg-configuration
           (keyboard-layout keyboard-layout)))
         (service docker-service-type))
   (modify-services %desktop-services
                    (guix-service-type config => (guix-configuration
                                                  (inherit config)
                                                  (substitute-urls
                                                   (append (list "https://substitutes.nonguix.org")
                                                           %default-substitute-urls))
                                                  (authorized-keys
                                                   (append (list (plain-file "non-guix.pub"
                                                                             "(public-key
 (ecc
  (curve Ed25519)
  (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))
                                                           %default-authorized-guix-keys))))
                    (gdm-service-type
                     config => (gdm-configuration
                                (gnome-shell-assets
                                 (list (specification->package "font-google-noto")
                                       (specification->package "adwaita-icon-theme"))))))))
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
