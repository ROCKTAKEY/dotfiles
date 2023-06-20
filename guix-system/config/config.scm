(use-modules (gnu)
             (gnu packages fonts)
             (gnu packages gnome)
             (nongnu packages linux)
             (nongnu packages nvidia)
             (nongnu system linux-initrd)
             (roquix services tailscale))
(use-service-modules desktop networking ssh xorg docker virtualization syncthing)

(operating-system
 (kernel
  ;; NOTE: NVIDIA driver 515 needs linux-5.15.
  linux-5.15)
 (kernel-loadable-modules (list nvidia-module))
 (kernel-arguments
  (list
   ;; NOTE: NVIDIA driver cannot be used with noveau and pcspkr
   "modprobe.blacklist=nouveau,pcspkr"))
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

   (specification->package "xrandr")

   (specification->package "i3-wm")
   (specification->package "i3status")
   (specification->package "rofi")
   (specification->package "polybar")

   %base-packages))
 (services
  (cons*
   (set-xorg-configuration
    (xorg-configuration
     (keyboard-layout keyboard-layout)
     (modules
      (cons*
       ;; NOTE: NVIDIA driver should be loaded as a kernel module
       nvidia-driver

       %default-xorg-modules))))

   ;; NOTE: NVIDIA driver needs udev rule to recognize NVIDIA GPU.
   (udev-rules-service 'nvidia-gpu nvidia-driver)

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
                  (append %default-substitute-urls
                          (list "https://substitutes.nonguix.org")))
                 (authorized-keys
                  (cons*
                   (plain-file "nonguix.pub"
                               "(public-key
 (ecc
  (curve Ed25519)
  (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))")
                   %default-authorized-guix-keys))))
     (gdm-service-type
      config => (gdm-configuration
                 (inherit config)
                 (auto-suspend? #f)
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
