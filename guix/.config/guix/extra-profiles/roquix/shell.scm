(use-modules (roquix extra-profiles shell-configuration))

(let ((home (getenv "HOME")))
  (shell-configuration
   (container? #t)
   (mounts
    (list (share (string-append home "/rhq/github.com/ROCKTAKEY/roquix"))))))
