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
             (roquix services tailscale))
(use-service-modules cups cuirass desktop networking ssh web xorg admin virtualization databases)

(define %cuirass-specs
  #~(list
     (specification
      (name "roquix")
      (build '(channels roquix))
      (channels
       (cons* (channel
               (name 'roquix)
               (url "https://github.com/ROCKTAKEY/roquix"))
              %default-channels)))
     (specification
      (name "guix-jp")
      (build '(channels guix-jp))
      (channels
       (cons* (channel
               (name 'guix-jp)
               (url "https://gitlab.com/guix-jp/channel")
               (branch "main"))
              %default-channels)))
     ;; (specification
     ;;  (name "my-channel")
     ;;  (build '(channels my-channel))
     ;;  (channels
     ;;   (cons (channel
     ;;          (name 'my-channel)
     ;;          (url "https://my-channel.git"))
     ;;         %default-channels)))
     ))

(operating-system
  (locale "ja_JP.utf8")
  (timezone "Asia/Tokyo")
  (keyboard-layout (keyboard-layout "jp,us"
                                    #:options '("grp:alt_shift_toggle")))
  (host-name "cuirass")

  ;; The list of user accounts ('root' is implicit).
  (users (cons* (user-account
                  (name "rocktakey")
                  (comment "ROCKTAKEY")
                  (group "users")
                  (home-directory "/home/rocktakey")
                  (supplementary-groups '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))

  ;; Below is the list of system services.  To search for available
  ;; services, run 'guix system search KEYWORD' in a terminal.
  (services
   (append (list (service dhcp-client-service-type)
                 (service ntp-service-type)
                 (service gpm-service-type)
                 (service cups-service-type)

                 (service tailscale-service-type)

                 (service unattended-upgrade-service-type)
                 (service qemu-guest-agent-service-type)
                 (service postgresql-service-type
                          (postgresql-configuration
                           (postgresql (@ (gnu packages databases) postgresql-15))))
                 (service cuirass-service-type
                          (cuirass-configuration
                           (specifications %cuirass-specs)
                           (host "127.0.0.1"))))

           ;; This is the default list of services we
           ;; are appending to.
           (modify-services %base-services
             (guix-service-type
                             config => (guix-configuration
                                        (inherit config)
                                        (substitute-urls
                                         (cons* "https://substitutes.nonguix.org"
                                                "https://guix.bordeaux.inria.fr"
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
                                          %default-authorized-guix-keys)))))))
  (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (targets (list "/dev/sda"))
                (keyboard-layout keyboard-layout)))
  (initrd-modules (append '("virtio_scsi") %base-initrd-modules))
  (swap-devices (list (swap-space
                        (target (uuid
                                 "40745685-e4af-443d-8bce-db35d143d92c")))))

  ;; The list of file systems that get "mounted".  The unique
  ;; file system identifiers there ("UUIDs") can be obtained
  ;; by running 'blkid' in a terminal.
  (file-systems (cons* (file-system
                         (mount-point "/")
                         (device (uuid
                                  "09f0235e-6ecd-4bee-aa4c-4fb8952c9c2b"
                                  'ext4))
                         (type "ext4")) %base-file-systems)))
