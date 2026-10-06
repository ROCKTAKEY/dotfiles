(use-modules (roquix extra-profiles shell-configuration)
             (srfi srfi-1))

(let ((runtime-directory (getenv "XDG_RUNTIME_DIR"))
      (wayland-display (getenv "WAYLAND_DISPLAY"))
      (xauthority (getenv "XAUTHORITY")))
  (define (share-runtime-socket name)
    (and name
         (not (string-null? name))
         (or runtime-directory (string-prefix? "/" name))
         (share (if (string-prefix? "/" name)
                    name
                    (string-append runtime-directory "/" name))
                #:on-missing 'skip)))

  (shell-configuration
   (container? #t)
   ;; VNC/SPICE consoles commonly listen on the host's loopback interface.
   ;; https://github.com/virt-manager/virt-manager/blob/main/man/virt-install.rst
   (network? #t)
   (preserve '("^XDG_RUNTIME_DIR$"
               "^WAYLAND_DISPLAY$"
               "^DBUS_SESSION_BUS_ADDRESS$"
               "^DISPLAY$"
               "^XAUTHORITY$"))
   (mounts
    (filter-map
     identity
     (list
      ;; The host daemon manages VM files; clients use its control socket.
      ;; https://libvirt.org/daemons.html
      (expose "/var/run/libvirt")
      ;; Share desktop sockets individually so profiles with an isolated
      ;; XDG runtime directory can mount them beneath their own directory.
      (share-runtime-socket "bus")
      (share-runtime-socket wayland-display)
      (expose "/tmp/.X11-unix" #:on-missing 'skip)
      (and xauthority
           (expose xauthority #:on-missing 'skip)))))))
