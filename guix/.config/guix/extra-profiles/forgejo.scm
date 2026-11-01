(use-modules (roquix extra-profiles shell-configuration))

(map (lambda (directory)
       (share (string-append (getenv "HOME") "/" directory)
              #:on-missing 'create-directory))
     '(".config/forgejo-cli" ".local/share/forgejo-cli" ".cache/git"))
