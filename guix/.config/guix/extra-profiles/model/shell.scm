(use-modules (roquix extra-profiles shell-configuration))

(let* ((home (getenv "HOME")))
  (shell-configuration
   (mounts
    (map (lambda (directory)
           (share (string-append home "/" directory)))
         '("models")))))
