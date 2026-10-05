(use-modules (roquix extra-profiles shell-configuration))

(let* ((home (getenv "HOME"))
       (project-directory (getcwd))
       (worktree-mounts
        (load (string-append (dirname (current-filename))
                             "/../git-worktree.scm")))
       (state-directory (string-append project-directory "/.guix-shell"))
       (runtime-directory
        (string-append "/run/user/" (number->string (getuid)))))
  (define (share-state-directory name target)
    (share (string-append state-directory "/" name)
           #:target target
           #:on-missing 'create-directory))

  (define (home-mounts)
    ;; At HOME, share the host home so it also remains the working directory.
    (if (string=? project-directory home)
        (list (share home))
        (append
         (list
          (share-state-directory "home" home)
          ;; Sharing HOME hides Guix's automatic project mount beneath it.
          ;; https://codeberg.org/guix/guix/src/branch/master/guix/scripts/environment.scm
          (share project-directory))
         (map (lambda (directory)
                (share (string-append home "/" directory)))
              '(".codex" ".agents" "dotfiles" ".config/git/ignore")))))

  (shell-configuration
   (container? #t)
   (network? #t)
   (nesting? #t)
   (writable-root? #f)
   (emulate-fhs? #t)
   (preserve '("^COLORTERM$"))
   (mounts
    (append
     (home-mounts)
     worktree-mounts
     (list (share-state-directory "tmp" "/tmp")
           (share-state-directory "runtime" runtime-directory))))))
