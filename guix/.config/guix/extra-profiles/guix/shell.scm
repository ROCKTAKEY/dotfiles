(use-modules (guix describe)
             (roquix extra-profiles shell-configuration)
             (srfi srfi-13))

(let* ((home (getenv "HOME"))
       (user (passwd:name (getpwuid (getuid))))
       (channel-modules
        (string-append home "/.config/guix/current/share/guile/site/"
                       (effective-version))))
  (shell-configuration
   (container? #t)
   (network? #t)
   (nesting? #t)
   ;; FHS /bin can select Guix outside the original pull profile, so channel
   ;; discovery also needs explicit module roots.
   (environment-variables
    (list (cons "GUIX_EXTENSIONS_PATH" channel-modules)
          (cons "GUIX_PACKAGE_PATH"
                (string-join (package-path-entries) ":"))))
   ;; Nesting provides Guix and its daemon socket. Host state must be mounted
   ;; beneath an isolated HOME so channels and profile updates persist.
   ;; https://guix.gnu.org/manual/en/html_node/Invoking-guix-shell.html
   (mounts
    (list
     (share (string-append home "/.config/guix"))
     ;; Stow links need their backing configuration tree inside the container.
     (share (dirname (dirname (dirname (canonicalize-path (current-filename))))))
     (share (string-append home "/.cache/guix")
            #:on-missing 'create-directory)
     (share (string-append home "/.guix-extra-profiles")
            #:on-missing 'create-directory)
     (share (string-append "/var/guix/profiles/per-user/" user))))))
