(use-modules (roquix extra-profiles shell-configuration))

(let* ((home (getenv "HOME"))
       (project (getcwd))
       (private-data (string-append home "/.local/share"))
       (host-data (or (getenv "XDG_DATA_HOME") private-data))
       (state (string-append project "/.guix-shell/")))
  (define (share-state name target)
    (share (string-append state name) #:target target
           #:on-missing 'create-directory))

  (shell-configuration
   (container? #t)
   (network? #t)
   (nesting? #t)
   (writable-root? #f)
   (emulate-fhs? #t)
   (preserve '("^COLORTERM$" "^OPENCODE_PASSWORD$"))
   (mounts
    (append
     (list (share-state "home" home))
     ;; The private HOME covers Guix's automatic project mount beneath it.
     (if (string=? project home) '() (list (share project)))
     ;; OpenCode 2 imports legacy auth.json credentials into its database.
     ;; Mount its directory so SQLite can also manage its journal files.
     (list (share (string-append host-data "/opencode")
                  #:target (string-append private-data "/opencode")
                  #:on-missing 'create-directory)
           (share-state "tmp" "/tmp")
           (share-state "runtime" (string-append "/run/user/" (number->string (getuid))))
           (expose (string-append home "/.config/git/config"))
           (expose (string-append home "/.config/git/config.local")
                   #:on-missing 'skip))
     (load (string-append (dirname (current-filename)) "/../forgejo.scm"))
     (load (string-append (dirname (current-filename)) "/../git-worktree.scm"))))))
