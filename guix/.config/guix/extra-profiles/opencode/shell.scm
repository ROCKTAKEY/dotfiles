(use-modules (roquix extra-profiles shell-configuration))

(shell-configuration
 (container? #t)
 (network? #t)
 (mounts
  (append
   (list (expose (string-append (getenv "HOME") "/.config/git/config"))
         (expose (string-append (getenv "HOME") "/.config/git/config.local")
                 #:on-missing 'skip))
   (load (string-append (dirname (current-filename)) "/../forgejo.scm"))
   (load (string-append (dirname (current-filename))
                        "/../git-worktree.scm")))))
