(use-modules (roquix extra-profiles shell-configuration))

(shell-configuration
 (mounts
  (list (share "/dev/net/tun"))))
