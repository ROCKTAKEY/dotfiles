(use-modules (gnu)
             (gnu packages fonts)
             (gnu packages gnome)
             (nongnu packages linux)
             (nongnu system linux-initrd)
             (roquix services tailscale))
(use-service-modules desktop networking ssh xorg docker virtualization syncthing)

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
   '("ctrl:nocaps"
     "caps:ctrl_modifier")))
 (host-name "guix-desktop")
 (users (cons* (user-account
                (name "rocktakey")
                (comment "ROCKTAKEY")
                (group "users")
                (home-directory "/home/rocktakey")
                (supplementary-groups
                 '("wheel" "netdev" "audio" "video" "docker" "kvm" "libvirt")))
               %base-user-accounts))
 (packages
  (cons*
   (specification->package "nss-certs")
   (specification->package "i3-wm")
   (specification->package "i3status")
   (specification->package "rofi")
   %base-packages))
 (services
  (cons*
   (set-xorg-configuration
    (xorg-configuration
     (keyboard-layout keyboard-layout)))
   (service gnome-desktop-service-type)
   (service syncthing-service-type
            (syncthing-configuration (user "rocktakey")))
   (service docker-service-type)
   (service libvirt-service-type
            (libvirt-configuration
             (unix-sock-group "libvirt")))
   (service virtlog-service-type)
   (service tailscale-service-type)
   (modify-services %desktop-services
     (guix-service-type
      config => (guix-configuration
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
                 (inherit config)
                 (gnome-shell-assets
                  (cons* (specification->package "font-google-noto")
                         (list adwaita-icon-theme font-abattis-cantarell)))))
     (network-manager-service-type
      config =>
      (network-manager-configuration
       (inherit config)
       (vpn-plugins (list
                     network-manager-openvpn
                     network-manager-vpnc
                     network-manager-openconnect)))))))
 (bootloader
  (bootloader-configuration
   (bootloader grub-bootloader)
   (targets '("/dev/sdb"))
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
