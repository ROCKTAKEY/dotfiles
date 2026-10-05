(use-modules (roquix extra-profiles shell-configuration))

(shell-configuration
 (container? #t)
 (mounts
  (load (string-append (dirname (current-filename))
                       "/../git-worktree.scm"))))
