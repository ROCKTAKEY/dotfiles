(use-modules (git)
             (guix git)
             (roquix extra-profiles shell-configuration))

;; A linked worktree stores both its private metadata and shared objects in
;; the parent repository, outside the working directory's container mount.
;; https://git-scm.com/docs/git-worktree#_details
(or (false-if-git-not-found
     (with-repository (repository-discover (getcwd)) repository
       (let ((common-directory
              (canonicalize-path (repository-common-directory repository))))
         (if (string=? common-directory
                       (canonicalize-path (repository-directory repository)))
             '()
             (list (share common-directory))))))
    '())
