(use-modules (roquix extra-profiles shell-configuration))

(shell-configuration
 (extra-options
  (if (file-exists? "./manifest.scm")
      '("-m" "./manifest.scm")
      '())))
