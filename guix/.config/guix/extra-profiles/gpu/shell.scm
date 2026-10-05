(use-modules (roquix extra-profiles shell-configuration))

(shell-configuration
 (mounts
  (list (share "/dev/kfd")
        (share "/dev/dri")
        (expose "/sys"))))
