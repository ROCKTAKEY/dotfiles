(use-modules (roquix extra-profiles shell-configuration)
             (srfi srfi-1))

(let ((runtime-directory (getenv "XDG_RUNTIME_DIR"))
      (xauthority (getenv "XAUTHORITY")))
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
      (and runtime-directory
           (share runtime-directory #:on-missing 'skip))
      (expose "/tmp/.X11-unix" #:on-missing 'skip)
      (and xauthority
           (expose xauthority #:on-missing 'skip)))))))
