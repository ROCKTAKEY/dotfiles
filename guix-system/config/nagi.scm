;; This is an operating system configuration generated
;; by the graphical installer.
;;
;; Once installation is complete, you can learn and modify
;; this file to tweak the system configuration, and pass it
;; to the 'guix system reconfigure' command to effect your
;; changes.


;; Indicate which modules to import to access the variables
;; used in this configuration.
(use-modules (gnu)
             (gnu packages fonts)
             (gnu packages gnome)
             (roquix services tailscale)
             (nongnu packages linux)
             (nongnu system linux-initrd))
(use-service-modules cups desktop networking ssh xorg docker virtualization syncthing nix)
(use-package-modules package-management wm)


(operating-system
  (kernel linux)
  (kernel-arguments (cons* "lsm=landlock"
                           %default-kernel-arguments))
  (initrd microcode-initrd)
  (firmware (list linux-firmware))
  (locale "ja_JP.utf8")
  (timezone "Asia/Tokyo")
  (keyboard-layout (keyboard-layout "jp"
                                    #:options
                                    '("ctrl:nocaps"
                                      "caps:ctrl_modifier")))
  (host-name "nagi")

  ;; The list of user accounts ('root' is implicit).
  (users (cons* (user-account
                  (name "rocktakey")
                  (comment "ROCKTAKEY")
                  (group "users")
                  (home-directory "/home/rocktakey")
                  (supplementary-groups '("wheel" "netdev" "audio" "video" "docker" "kvm" "libvirt"
                                          ;; Use xremap without sudo
                                          "input")))
                %base-user-accounts))

  ;; Packages installed system-wide.  Users can also install packages
  ;; under their own account: use 'guix search KEYWORD' to search
  ;; for packages and 'guix install PACKAGE' to install a package.
  (packages (append (list (specification->package "openbox")
                          (specification->package "awesome")
                          (specification->package "i3-wm")
                          (specification->package "i3status")
                          (specification->package "dmenu")
                          (specification->package "ratpoison")
                          (specification->package "xterm")
                          (specification->package "sway")
                          (specification->package "font-cica")
                          (specification->package "st")
                          (specification->package "font-google-noto")
                          (specification->package "font-google-noto-sans-cjk")
                          (specification->package "font-google-noto-serif-cjk")
                          nix
                          swaylock)
                    %base-packages))

  ;; Below is the list of system services.  To search for available
  ;; services, run 'guix system search KEYWORD' in a terminal.
  (services
   (append (list (service gnome-desktop-service-type)
                 (service xfce-desktop-service-type)
                 (service enlightenment-desktop-service-type)
                 (service screen-locker-service-type
                          (screen-locker-configuration
                           ;; the `name' property must be same as the name of the executable
                           (name "i3lock")
                           (program (file-append i3lock-color "/bin/i3lock"))))
                 (service screen-locker-service-type
                          (screen-locker-configuration
                           ;; the `name' property must be same as the name of the executable
                           (name "swaylock")
                           (program (file-append swaylock-effects "/bin/swaylock"))
                           (using-pam? #t)
                           (using-setuid? #f)))
                 (service cups-service-type)
                 (set-xorg-configuration
                  (xorg-configuration
                   (keyboard-layout keyboard-layout)
                   (extra-config (list "
Section \"InputClass\"
  Identifier \"Change Mouse Speed\"
  MatchDriver \"libinput\"
  MatchProduct \"ELECOM CO., LTD. ELECOM OpticalMouse\"
  Option \"AccelSpeed\" \"-0.8\"
EndSection
"))))

                 (service iptables-service-type
                          (iptables-configuration
                           (ipv4-rules (plain-file "iptables.rules" "*filter
:INPUT ACCEPT
:FORWARD ACCEPT
:OUTPUT ACCEPT
-A INPUT -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT
-A INPUT -i lo -j ACCEPT
-A INPUT -p ipv6-icmp -j ACCEPT
-A INPUT -j REJECT --reject-with icmp-port-unreachable
COMMIT
"))
                           (ipv6-rules (plain-file "ip6tables.rules" "*filter
:INPUT ACCEPT
:FORWARD ACCEPT
:OUTPUT ACCEPT
-A INPUT -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT
-A INPUT -i lo -j ACCEPT
-A INPUT -p ipv6-icmp -j ACCEPT
-A INPUT -j REJECT --reject-with icmp6-port-unreachable
COMMIT
"))))

                 (service syncthing-service-type
                          (syncthing-configuration (user "rocktakey")))
                 (service containerd-service-type)
                 (service docker-service-type)
                 (service libvirt-service-type
                          (libvirt-configuration
                           (unix-sock-group "libvirt")))
                 (service virtlog-service-type)
                 (service qemu-binfmt-service-type
                          (qemu-binfmt-configuration
                           (platforms (lookup-qemu-platforms "arm" "aarch64"))))

                 (service tailscale-service-type)
                 (service nix-service-type))

           ;; This is the default list of services we
           ;; are appending to.
           (modify-services %desktop-services
                            (guix-service-type
                             config => (guix-configuration
                                        (inherit config)
                                        (substitute-urls
                                         (cons* "https://substitutes.nonguix.org"
                                                "https://guix.bordeaux.inria.fr"
                                                "https://cuirass.tail09c0.ts.net"
                                                %default-substitute-urls))
                                        (authorized-keys
                                         (cons*
                                          (plain-file "nonguix.pub"
                                                      "\
(public-key
 (ecc
  (curve Ed25519)
  (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))\
"
                                                      )
                                          (plain-file "inria.pub"
                                                      "\
(public-key
 (ecc
  (curve Ed25519)
  (q #89FBA276A976A8DE2A69774771A92C8C879E0F24614AAAAE23119608707B3F06#)))\
"
                                                      )
                                          (plain-file "cuirass.pub"
                                                      "(public-key
 (ecc
  (curve Ed25519)
  (q #9EF17DBD1777BF3BA48A7E5931C01FB1C28335D625D192E85170F163F2EB422C#)
  )
 )")
                                          %default-authorized-guix-keys))))
                            (gdm-service-type
                             config => (gdm-configuration
                                        (inherit config)
                                        (auto-suspend? #f)
                                        (wayland? #t)
                                        (gnome-shell-assets
                                         (list (specification->package "font-google-noto")
                                               (specification->package "font-google-noto-sans-cjk")
                                               (specification->package "font-google-noto-serif-cjk")
                                               (specification->package "adwaita-icon-theme")))))
                            (network-manager-service-type
                             config =>
                             (network-manager-configuration
                              (inherit config)
                              (vpn-plugins (list
                                            network-manager-openvpn
                                            network-manager-vpnc
                                            network-manager-openconnect))))
                            (udev-service-type
                             config =>
                             (udev-configuration
                              (inherit config)
                              (rules
                               (list
                                ;; Use xremap without sudo
                                (udev-rule
                                 "50-xremap.rules"
                                 "KERNEL==\"uinput\", GROUP=\"input\", TAG+=\"uaccess\""))))))))
  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets (list "/boot/efi"))
                (keyboard-layout keyboard-layout)))
  (mapped-devices (list (mapped-device
                          (source (uuid
                                   "65d2b709-62e5-4a86-8aad-6f142044ab31"))
                          (target "cryptroot")
                          (type luks-device-mapping))))

  ;; The list of file systems that get "mounted".  The unique
  ;; file system identifiers there ("UUIDs") can be obtained
  ;; by running 'blkid' in a terminal.
  (file-systems (cons* (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "944D-FB38"
                                       'fat32))
                         (type "vfat"))
                       (file-system
                         (mount-point "/")
                         (device "/dev/mapper/cryptroot")
                         (type "ext4")
                         (dependencies mapped-devices)) %base-file-systems)))
