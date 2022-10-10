;; (package-initialize)

(gv-define-expander plist-get
  (lambda (do plist prop)
    (macroexp-let2 macroexp-copyable-p key prop
      (gv-letplace (getter setter) plist
        (macroexp-let2 nil p `(plist-member ,getter ,key)
          (funcall do
                   `(cadr ,p)
                   (lambda (val)
                     `(if (plist-member ,plist ,key) (setcar (cdr (plist-member ,plist ,key)) ,val)
                        ,(funcall setter `(cons ,key (cons ,val ,getter)))))))))))

;; load-path

(add-to-list 'load-path (expand-file-name "conf" user-emacs-directory))



(require 'cl-lib)

(prog1 'nsm
  (set-variable
   'nsm-settings-file
   (expand-file-name
    "etc/network-security.data"
    user-emacs-directory)))

(prog1 'package
  (require 'package)
  (setq package-check-signature nil)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
  (add-to-list 'package-archives '("celpa" . "https://celpa.conao3.com/packages/"))
  (add-to-list 'package-archives '("roquelpa" . "https://rocktakey.github.io/roquelpa/"))
  (package-initialize))

(unless (package-installed-p 'leaf)
  (package-refresh-contents)
  (package-install 'leaf))

(eval-and-compile
  (leaf leaf
    :require t
    :custom `((leaf-defaults . '(:ensure t)))
    :config
    (eval-and-compile
      (defmacro leaf* (name &rest args)
        "leaf without ensureing."
        (declare (indent defun))
        `(leaf ,name :ensure nil :leaf-defer nil ,@args)))
    (font-lock-add-keywords
     'emacs-lisp-mode
     '(("(\\(leaf\\*?\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
        (1 font-lock-keyword-face)
        (2 font-lock-constant-face nil t)))))

  (leaf leaf-keywords
    :require t
    :defvar
    (leaf-keywords-before-conditions
     leaf-keywords-documentation-keywords
     leaf-keywords-after-conditions
     leaf-keywords-after-config
     leaf-keywords-before-protection
     leaf-keywords-before-require
     leaf-keywords-normalize)
    :init
    (leaf straight
      :ensure nil
      :init
      (let ((bootstrap-file
             (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
            (bootstrap-version 3))
        (unless (file-exists-p bootstrap-file)
          (with-current-buffer
              (url-retrieve-synchronously
               "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
               'silent 'inhibit-cookies)
            (goto-char (point-max))
            (eval-print-last-sexp)))
        (load bootstrap-file nil 'nomessage))
      :custom
      (straight-vc-git-default-clone-depth . 1))

    (leaf el-get
      :custom ((el-get-git-shallow-clone . t)))

    (leaf hydra
      :require t)

    (leaf mykie
      :doc
      "Fusion key bindings with prefix arguments and so on."
      :require t)
    :config
    (mapc
     (lambda (arg)
       (setf (plist-get leaf-keywords-before-conditions (car arg))
             (cadr arg)))
     '((:system-type
        (when leaf--body
          `((when
                (memq system-type
                      ',(if (listp (car leaf--value))
                            (car leaf--value)
                          leaf--value))
              ,@leaf--body))))
       (:system-name
        (when leaf--body
          `((when
                (member (system-name)
                        ',(if (listp (car leaf--value))
                              (car leaf--value)
                            leaf--value))
              ,@leaf--body))))
       (:system-name-regexp
        (when leaf--body
          `((when
                (cl-some (lambda (arg)
                           (string-match arg (system-name)))
                         ',(if (listp (car leaf--value))
                               (car leaf--value)
                             leaf--value))
              ,@leaf--body))))))

    (plist-put leaf-keywords-documentation-keywords
               :defer '`(,@leaf--body))
    (add-to-list 'leaf-defer-keywords :defer)

    (plist-put
     leaf-keywords-before-require
     :mykie
     '(let ((map (gensym))
            (after-load (gensym))
            rec fns)
        (set map 'global-map)
        (set after-load nil)
        (setq
         rec
         (lambda (arg)
           (setq arg (car arg))
           (cond
            ((and (listp arg) (not (stringp (car arg))))
             (let ((map (gensym))
                   (after-load (gensym)))
               (set map 'global-map)
               (set after-load nil)
               (cl-mapcon rec arg)))
            ((eq (symbol-value after-load) t)
             (set after-load arg)
             nil)
            ((eq arg :package)
             (set after-load t)
             nil)
            ((and (symbolp arg) (string-match "^:\\(.*\\)" (symbol-name arg)))
             (set map (intern (substring (symbol-name arg) 1)))
             nil)
            (t
             (setf
              (cdr arg)
              (cl-mapcon
               (lambda (arg)
                 (when (cl-oddp (length arg))
                   (cl-pushnew (car arg) fns))
                 `(,(car arg)))
               (cdr arg)))
             (if (and (symbol-value after-load)
                      (not (eq (symbol-value after-load) t)))
                 (list
                  `(with-eval-after-load ',(symbol-value after-load)
                     (mykie:define-key ,(symbol-value map) ,(car arg) ,@(cdr arg))))
               (list `(mykie:define-key ,(symbol-value map) ,(car arg) ,@(cdr arg))))))))

        (setq result (cl-mapcon rec leaf--value))
        (append
         (mapcar
          (lambda (arg)
            `(autoload ',arg ,(symbol-name leaf--name) nil t))
          (nreverse fns))
         result
         leaf--body)))
    (add-to-list 'leaf-defer-keywords :mykie)

    (setf (plist-get leaf-keywords-after-conditions :depends)
          '`(,@(mapcar
                (lambda (elm)
                  `(leaf-handler-package ,leaf--name ,(car elm) ,(cdr elm)))
                (if (listp (car leaf--value)) leaf--value (list leaf--value)))
             ,@leaf--body))

    (require 'plstore)

    (setf
     (plist-get leaf-keywords-after-config :pl-post-custom)
     '`(,@(mapcar (lambda (elm)
                    `(customize-set-variable
                      ',(car elm)
                      (leaf-handler-auth ,leaf--name ,(car elm) ,(cdr elm))
                      ,(leaf--create-custom-comment :pl-custom (cdr elm))))
                  leaf--value)
        ,@leaf--body))

    (push
     '((eq leaf--key :pl-post-custom)
       (mapcar (lambda (elm)
                 (cond
                  ((leaf-pairp elm)
                   (if (eq t (car elm)) `(,leaf--name . ,(cdr elm)) elm))
                  (t
                   `(,@elm . leaf-default-plstore))))
               (mapcan
                (lambda (elm) (leaf-normalize-list-in-list elm 'dotlistp))
                leaf--value)))
     leaf-keywords-normalize)


    (leaf-keywords-init))

  (leaf series
      :ensure nil
      :el-get ROCKTAKEY/series
      :require t))

(leaf leaf-tree
  :custom
  `(leaf-tree-regexp
    . ,(concat
        "^\\s-*(\\_<\\(leaf\\*?\\)\\_>\\s-+\\("
        (or (bound-and-true-p lisp-mode-symbol-regexp)
            "\\(?:\\sw\\|\\s_\\|\\\\.\\)+")
        "\\)")))

(leaf* userinfo
  :custom
  ((user-mail-address . "rocktakey@gmail.com"))
  :config
  (leaf startup
    :ensure nil
    :custom
    (user-full-name . "ROCKTAKEY")))

(eval-and-compile
  (leaf* defvar
    :config
    (defvar my-dropbox-directory
      (expand-file-name
       "Dropbox"
       (pcase (list system-type (getenv "WSL_DISTRO_NAME"))
         (`(windows-nt ,_) (getenv "USERPROFILE"))
         (`(gnu/linux ,(pred identity))
          (cl-some
           (lambda (arg)
             (and
              (not
               (cl-some
                (apply-partially #'string= (file-name-nondirectory arg))
                '("." ".." "All Users" "Default" "Default User" "desktop.ini" "Public")))
              (file-accessible-directory-p arg)
              (file-readable-p arg)
              (file-writable-p arg)
              arg))
           (directory-files "/mnt/c/Users/" t)))
         (_ (getenv "HOME")))))
    (defvar my-org-directory
      (expand-file-name "memo" my-dropbox-directory))
    (defvar my-blog-directory
      (expand-file-name (concat user-full-name "/" "blog")
                        "~/rhq/github.com"))))

(leaf* requirement-for-setting
  :config
  (leaf* macro
    :config
    (leaf shut-up
      :doc
      "Shut up all messages under `shut-up'"
      :preface
      (defmacro def-shut-up-advice (func)
        "Make FUNC shut-up using advice."
        `(defadvice ,func
             (around ,(intern (concat (symbol-name func) "-shut-up")) activate)
           (shut-up ad-do-it)))
      :commands shut-up)
    (leaf dash :require t))

  (leaf region-bindings-mode
    :require t
    :defun region-bindings-mode-enable
    :config
    (region-bindings-mode-enable))

  ;; (add-hook 'switch-buffer-functions
  ;;           (lambda (prev cur)
  ;;             (message "%S -> %S" (buffer-name prev) (buffer-name cur))))
  (leaf switch-buffer-functions
    :require t
    :preface
    (defvar switch-buffer-hook nil
      "Hook which called like `switch-buffer-functions'.
No arguments is passed.")
    (defun run-switch-buffer-hook (_ __)
      (run-hooks 'switch-buffer-hook))
    :hook
    ((switch-buffer-functions . run-switch-buffer-hook))))

(leaf* authority
  ;; port:   pacage name
  ;; host:   url
  ;; user:   user id
  ;; secret: passward
  :config
  (leaf epg
    :custom
    ((epg-gpg-program . "gpg")
     (epg-pinentry-mode . 'loopback)))

  (leaf pinentry
    :if (eq system-type 'gnu/linux)
    :config
    (pinentry-start))

  (leaf auth-source
    :ensure nil
    :custom
    `((auth-sources
       . ',(list
            (expand-file-name "etc/authinfo.gpg"   user-emacs-directory)
            (expand-file-name "etc/authinfo.plist" user-emacs-directory)))))

  (leaf epa)

  (leaf plstore
    :mode ("\\.plist\\'" . plstore-mode)
    :custom ((plstore-encoded . t)))

  (leaf uuid
    :commands uuid-string))

(defgroup mine nil "My group."
  :group 'lisp)

(leaf* browse/url
  :config
  (leaf url
    :ensure nil
    :defun url-do-setup
    :custom
    `((url-configuration-directory
       . ,(expand-file-name
           "etc/url/"
           user-emacs-directory)))
    :preface
    (defun my:url-retrieve-synchronously (url &optional silent inhibit-cookies timeout)
  "Retrieve URL synchronously.
Return the buffer containing the data, or nil if there are no data
associated with it (the case for dired, info, or mailto URLs that need
no further processing).  URL is either a string or a parsed URL.

If SILENT is non-nil, don't do any messaging while retrieving.
If INHIBIT-COOKIES is non-nil, refuse to store cookies.  If
TIMEOUT is passed, it should be a number that says (in seconds)
how long to wait for a response before giving up."
  (url-do-setup)

  (let ((retrieval-done nil)
	(start-time (current-time))
        (url-asynchronous nil)
        (asynch-buffer nil))
    (setq asynch-buffer
	  (url-retrieve url (lambda (&rest ignored)
			      (url-debug 'retrieval "Synchronous fetching done (%S)" (current-buffer))
			      (setq retrieval-done t
				    asynch-buffer (current-buffer)))
			nil silent inhibit-cookies))
    (if (null asynch-buffer)
        ;; We do not need to do anything, it was a mailto or something
        ;; similar that takes processing completely outside of the URL
        ;; package.
        nil
      (let ((proc (get-buffer-process asynch-buffer)))
	;; If the access method was synchronous, `retrieval-done' should
	;; hopefully already be set to t.  If it is nil, and `proc' is also
	;; nil, it implies that the async process is not running in
	;; asynch-buffer.  This happens e.g. for FTP files.  In such a case
	;; url-file.el should probably set something like a `url-process'
	;; buffer-local variable so we can find the exact process that we
	;; should be waiting for.  In the mean time, we'll just wait for any
	;; process output.
	(while (and proc (not retrieval-done)
                    (or (not timeout)
			(time-less-p (time-since start-time) timeout)))
	  (url-debug 'retrieval
		     "Spinning in url-retrieve-synchronously: %S (%S)"
		     retrieval-done asynch-buffer)
          (if (buffer-local-value 'url-redirect-buffer asynch-buffer)
              (setq proc (get-buffer-process
                          (setq asynch-buffer
                                (buffer-local-value 'url-redirect-buffer
                                                    asynch-buffer))))
            (if (and proc (memq (process-status proc)
                                '(closed exit signal failed))
                     ;; Make sure another process hasn't been started.
                     (eq proc (or (get-buffer-process asynch-buffer) proc)))
                ;; FIXME: It's not clear whether url-retrieve's callback is
                ;; guaranteed to be called or not.  It seems that url-http
                ;; decides sometimes consciously not to call it, so it's not
                ;; clear that it's a bug, but even then we need to decide how
                ;; url-http can then warn us that the download has completed.
                ;; In the mean time, we use this here workaround.
		;; XXX: The callback must always be called.  Any
		;; exception is a bug that should be fixed, not worked
		;; around.
		(progn ;; Call delete-process so we run any sentinel now.
		  (delete-process proc)
		  (setq retrieval-done t)))
            ;; We used to use `sit-for' here, but in some cases it wouldn't
            ;; work because apparently pending keyboard input would always
            ;; interrupt it before it got a chance to handle process input.
            ;; `sleep-for' was tried but it lead to other forms of
            ;; hanging.  --Stef
            (unless (or (with-local-quit
			  (accept-process-output proc 1))
			(null proc))
              ;; accept-process-output returned nil, maybe because the process
              ;; exited (and may have been replaced with another).  If we got
	      ;; a quit, just stop.
	      (when quit-flag
		(delete-process proc))
              (setq proc (and (not quit-flag)
			      (get-buffer-process asynch-buffer)))))))
      asynch-buffer)))
    :advice
    (:override url-retrieve-synchronously my:url-retrieve-synchronously)
    :config
    (defun set-url-proxy (server-name)
      "set http proxy"
      (interactive "shttp proxy server: ")
      (setq url-proxy-services `(("http" . ,server-name)))
      (setenv "http_proxy" server-name)))

  (leaf eww
    :bind
    (:eww-mode-map
     ("u" . eww-copy-page-url)
     ("n" . next-line)
     ("p" . previous-line))
    :hook
    ((eww-mode-hook . trailing-whitespace-off))
    :custom
    ((eww-search-prefix . "http://www.google.co.jp/search?q=")
     (eww-download-directory . "~/../../downloads/"))))

(leaf* delsel
  :global-minor-mode
  delete-selection-mode)

(leaf simple
  :ensure nil
  :bind
  (("C-d" . delete-forward-char)
   ("C-x l" . toggle-truncate-lines))
  :preface
  (defun ad:transpose-chars (&optional arg)
    "Same as `transpose-chars' except always use 2 chars before cursor."
    (interactive "P")
    (and (null arg) (forward-char -1))
    (transpose-subr 'forward-char (prefix-numeric-value arg)))
  :advice
  (:override transpose-chars ad:transpose-chars)
  :global-minor-mode
  column-number-mode
  line-number-mode
  :custom
  ((kill-whole-line . t)
   (set-mark-command-repeat-pop . t)
   (mark-ring-max . 50))
  :hook
  (before-save-hook . delete-trailing-whitespace))

(leaf cus-edit
  :ensure nil
  :custom
  `((custom-file . ,(expand-file-name "etc/custom.el" user-emacs-directory))))

(leaf* load
  :config
  (setq load-prefer-newer t))

(leaf server
  :custom
  ;; emacsclient
  `((server-auth-dir
     . ,(expand-file-name
         "etc/server/"
         user-emacs-directory)))
  :hook
  (after-init-hook . server-start))

(leaf files
  :ensure nil
  :preface
  (defun rename-file-and-buffer (new-name)
    "Renames both current buffer and file it's visiting to NEW-NAME."
    (interactive (list (read-string "New name: " (buffer-name))))
    (let ((name (buffer-name))
          (filename (buffer-file-name)))
      (if (not filename)
          (message "Buffer '%s' is not visiting a file!" name)
        (if (get-buffer new-name)
            (message "A buffer named '%s' already exists!" new-name)
          (progn
            (rename-file filename new-name 1)
            (rename-buffer new-name)
            (set-visited-file-name new-name)
            (set-buffer-modified-p nil))))))
  :mykie
  (("C-x C-s" :default save-buffer :C-u save-some-buffers))
  :custom
  (confirm-kill-emacs . #'y-or-n-p)
  (require-final-newline . t)
  (view-read-only . t))

(leaf window
  :ensure nil
  :bind*
  ("C-o" . other-window)
  :hydra
  (hydra-window-resizer
   nil
   ("p" shrink-window "shrink")
   ("n" enlarge-window "enlarge")
   ("f" enlarge-window-horizontally "enlarge-horizontally")
   ("b" shrink-window-horizontally "shrink-horizontally")
   ;; ("k" shrink-window)
   ;; ("j" enlarge-window)
   ;; ("l" enlarge-window-horizontally)
   ;; ("h" shrink-window-horizontally)
   ("<down>" shrink-window)
   ("<up>" enlarge-window)
   ("<right>" enlarge-window-horizontally)
   ("<left>" shrink-window-horizontally)
   ("q" nil "quit"))
  :mykie
  ("C-w" :default hydra-window-resizer/body :region kill-region)
  :config
  (leaf swap-buffers
    :defun
    (swap-buffers)
    :bind (("C-x w" . swap-buffers-keep-focus))
    :preface
    (defun swap-buffers-keep-focus ()
      (interactive)
      (swap-buffers t)))

  (leaf ace-window
    :defun
    (aw-switch-to-window
     aw-select)
    :bind*
    ("M-o" . ace-window)
    :custom
    ((aw-keys . '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?\;))
     (aw-dispatch-always . t))
    :custom-face
    ((aw-leading-char-face
      . '((t (:foreground "red" :height 10.0)))))
    ((aw-mode-line-face
      . '((t (:background "#006000" :foreground "white" :bold t)))))
    :global-minor-mode ace-window-display-mode))

(leaf uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style . 'post-forward-angle-brackets)
  (uniquify-ignore-buffers-re . "*[^*]"))

(leaf keyfreq
  :global-minor-mode
  keyfreq-mode
  keyfreq-autosave-mode
  :custom
  `((keyfreq-file . ,(expand-file-name "etc/emacs.keyfreq" user-emacs-directory))))

(leaf auto-revert
  :ensure nil
  :global-minor-mode
  global-auto-revert-mode)

(leaf* sounds
  :config
  (setq ring-bell-function 'ignore))

(leaf startup
  :ensure nil
  :custom
  ((inhibit-startup-message . t)
   (initial-scratch-message . "")))

(leaf* indent
  :custom
  ((tab-width . 4)
   (indent-tabs-mode . nil)))

(leaf* bar
  :config
  (leaf menu-bar
    :ensure nil
    :config
    (menu-bar-mode -1))

  (leaf tool-bar
    :ensure nil
    :config
    (tool-bar-mode -1)))

(leaf* scroll
  :custom
  ((scroll-conservatively . 35)
   (scroll-margin . 0)
   (scroll-step . 1))
  :config
  (leaf scroll-bar
    :ensure nil
    :config
    (scroll-bar-mode -1)))

(leaf frame
  :ensure nil
  :custom
  ((blink-cursor-blinks . 0)
   (frame-title-format
    . '(global-mode-string
        (:eval (if (buffer-file-name)
                   "%f" "%b")))))
  :config
  (global-set-key (kbd "C-z") nil))

(leaf* time
  :custom
  (display-time-day-and-date . t)
  (display-time-format . "%Y-%m-%d %a %-H:%M")
  (display-time-default-load-average . nil)
  (system-time-locale . "C"))

(leaf* c-source-vars
  :custom
  (create-lockfiles . nil))

(leaf* lisp
  :custom
  ((max-lisp-eval-depth . 5000)
   (max-specpdl-size . 5000))
  :config
  (leaf package-lint))

(leaf paradox
  :bind ((package-menu-mode-map
          ("R" . package-reinstall)))
  :preface
  (leaf-keys
   (("C-x p l" . paradox-list-packages)
    ("C-x p r" . package-refresh-contents)))
  :commands
  paradox-list-packages
  package-refresh-contents
  :custom
  ((paradox-execute-asynchronously . nil)
   (paradox-automatically-star . nil)
   (paradox-column-width-version . 13)
   (paradox-display-star-count . nil)
   (paradox-github-token . t))
  :config
  (paradox-enable))

(leaf async
  :defun
  my-async-variables-noprops
  :preface
  (defun my-async-variables-noprops (sequence)
    "Remove text properties in SEQUENCE.

Argument SEQUENCE may be a list or a string, if anything else it
is returned unmodified.

Note that this is a naive function that \"DOES\" remove text properties
in SEQUENCE \"RECURSIVELY\", only at the first level which suffice in most
cases."
    (cond ((stringp sequence)
           (substring-no-properties sequence))
          ((listp sequence)
           (cl-loop for elm in sequence
                    if (stringp elm)
                    collect (substring-no-properties elm)
                    else collect (my-async-variables-noprops elm)))
          (t sequence)))
  :custom
  ((async-variables-noprops-function . #'my-async-variables-noprops)))

(leaf multi-compile
  :bind
  ((:c-mode-map
    :package cc-mode
    ("C-c c" . multi-compile-run))
   (:c++-mode-map
    :package cc-mode
    ("C-c c" . multi-compile-run))
   (:typescript-mode-map
    :package typescript-mode
    ("C-c c" . multi-compile-run)))
  :custom
  `((multi-compile-history-file
     . ,(expand-file-name
         "etc/multi-compile.cache"
         user-emacs-directory))
    (multi-compile-alist
     . '(
         ;; %make-dir is directory which includes Makefile
         ;; (See `multi-compile-template')
         (c-mode . (("plain" . "gcc -o %file-sans -Wall %path")
                    ("make this" . "make %file-sans")))
         (c++-mode
          . (("compile with debug info" .
              ,(concat
                "g++ -g3 -O0  %path -Wall -Wnon-virtual-dtor"
                " -Woverloaded-virtual -std=c++17"))
             ("compile plane" .
              ,(concat
                "g++ -o %file-sans %path -O2 -Wall "
                "-Wnon-virtual-dtor -Woverloaded-virtual -std=c++17"))
             ("make" .
              "make --no-print-directory -C %make-dir")
             ("atcoder" . "g++ -g3 -std=gnu++17 -Wall -Wextra -O0 %path -fsanitize=address -fsanitize=undefined")))
         (typescript-mode
          . (("plain" .
              "tsc %path"))))))
  :preface
  (defvar before-multi-compile-hook nil)
  (defun ad:multi-compile-run ()
    (run-hooks 'before-multi-compile-hook))
  :advice
  ((:before multi-compile-run ad:multi-compile-run))
  :hook
  ((before-multi-compile-hook . save-buffer))
  :defvar multi-compile-template
  :config
  (add-to-list
   'multi-compile-template
   '("%proj" . (project-root (project-current)))))

(leaf imenu
  :ensure nil
  :defun lsp-ui-imenu
  :defvar lsp-mode
  :custom
  (imenu-auto-rescan . t)
  :preface
  (defun my-imenu ()
    (interactive)
    (call-interactively
     (if (and (functionp #'lsp-ui-imenu)
              lsp-mode)
         #'lsp-ui-imenu
       #'imenu-list)))
  :bind (("M-i" . my-imenu))
  :config
  (leaf imenu-list
    :custom
    (imenu-list-position . 'left)
    :custom-face
    (imenu-list-entry-face
     . '((t (:width condensed :height 120))))
    (imenu-list-entry-subalist-face-0
     . '((t (:width expanded :height 130))))))

(leaf repeep
  :ensure nil
  :el-get ROCKTAKEY/repeep
  :bind (("C-=" . repeep))
  :global-minor-mode repeep-macro-mode)

(leaf lsp-mode
  :emacs>=  "25.1"
  :hook ((c-mode-hook           . lsp)
         (c++-mode-hook         . lsp)
         (web-mode-hook         . lsp)
         (dockerfile-mode-hook  . lsp)
         (yaml-mode-hook        . lsp)
         (r-mode-hook           . lsp)
         (javascript-mode-hook  . lsp)
         (js-mode-hook          . lsp)
         (typescript-mode-hook  . lsp)
         (sh-mode-hook          . lsp)
         (common-lisp-mode-hook . lsp)
         (yatex-mode-hook       . lsp)
         (tex-mode-hook         . lsp)
         (latex-mode-hook       . lsp)
         (bibtex-mode-hook      . lsp)
         (java-mode-hook        . lsp)
         (clojure-mode-hook     . lsp)
         (rust-mode-hook        . lsp))
  :defun (lsp-register-client make-lsp-client lsp-stdio-connection)
  :defvar (lsp--formatting-indent-alist)
  :bind
  (lsp-mode-map
   ("M-r" . lsp-rename)
   ("M-c" . lsp-execute-code-action))
  :custom
  `((lsp-session-file
     . ,(expand-file-name
         "etc/.lsp-session-v1"
         user-emacs-directory))
    (lsp-log-io . nil)
    (lsp-log-max . nil))
  :preface
  ;; shut-up view-mode message on lsp-mode.
  (defun ad:view-lsp (orig &rest aaa)
    (if (string= (buffer-name) " *temp*")
        (shut-up
          (apply orig aaa))
      (apply orig aaa)))
  :advice (:around view-mode-enter ad:view-lsp)
  :config
  (add-to-list 'lsp--formatting-indent-alist '(web-mode . web-mode-code-indent-offset))

  (leaf lsp-ui
    :hook (lsp-mode-hook . lsp-ui-mode)
    :bind
    (:lsp-ui-mode-map
     ("M-d" . lsp-ui-doc-mode))
    :custom
    ((lsp-ui-sideline-ignore-duplicate . t)
     (lsp-ui-sideline-show-hover . t))
    :custom-face
    (lsp-ui-sideline-symbol-info
     . '((t (:foreground "#6060dd" :background nil)))))

  (leaf ccls
    :require t
    :after cc-mode)

  (leaf lsp-latex
    :require t)

  (leaf lsp-java
    :custom
    `((lsp-java-project-referenced-libraries
       . ["lib/**/*.jar" ,(expand-file-name "~/Fiji.app/jars/*.jar")])))

  (leaf dap-mode
    :custom
    `((dap-breakpoints-file
       . ,(expand-file-name "etc/.dap-breakpoints" user-emacs-directory)))))

(leaf tree-sitter
  :defvar
  tree-sitter-major-mode-language-alist
  :ensure tree-sitter-langs
  :require tree-sitter-langs
  :global-minor-mode global-tree-sitter-mode
  :hook ((tree-sitter-after-on-hook . tree-sitter-hl-mode))
  :config
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-mode . tsx)))

(leaf ibuffer
  :bind ("C-x C-b" . ibuffer)
  :custom
  (ibuffer-formats
   . '((mark  vc-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size 9 -1 :right)
              "  "
              (mode 16 16 :left :elide)
              " "
              (vc-status 16 16 :left)
              " "
              filename-and-process)))
  :config
  (leaf ibuffer-vc
    :commands ibuffer-vc-set-filter-groups-by-vc-root
    :defvar
    ibuffer-sorting-mode
    :defun ibuffer-do-sort-by-alphabetic
    :preface
    ;; Sort by repository
    (defun my-ibuffer-hook ()
      (ibuffer-vc-set-filter-groups-by-vc-root)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic)))
    :hook
    (ibuffer-hook . my-ibuffer-hook)))

(leaf quickrun
  :config
  (quickrun-add-command "c++/g++/c++17"
    '((:command . "g++")
      (:exec    . ("%c -std=c++17 %o -o %e %s -g3 -O0"
                   "%e %a"))
      (:remove  . ("%e"))
      (:description . "Compile C++ file with g++ -std=c++17 and execute"))
    :default "c++"))

(leaf npm)

(leaf* major-mode
  :config
  (leaf* biology
    :config
    (leaf pdb-mode
      :mode (("\\.pdb\\'" . pdb-mode))))

  (leaf cask-mode)

  (leaf csv-mode)

  (leaf markdown-mode)

  (leaf cc-mode
    :ensure nil
    :defun c-lineup-math my:c-statement-cont
    :mode ((".*/include/c\\+\\+/.*" . c++-mode))
    :custom
    ((c-basic-offset . 4)
     (c-default-style . "bsd"))
    :config
    (defun my:c-statement-cont (cons)
      (if (string=
           (let ((start (save-excursion
                          (goto-char (cdr cons))
                          (beginning-of-line-text)
                          (point))))
             (buffer-substring-no-properties start (+ start 6)))
           "return")
          7
        (c-lineup-math cons)))
    (c-set-offset 'statement-cont #'my:c-statement-cont)
    (leaf modern-cpp-font-lock
      :hook (c++-mode-hook . modern-c++-font-lock-mode))
    (leaf clang-format+
      :hook
      (c++-mode-hook . clang-format+-mode)))

  (leaf* common-lisp
    :config
    (leaf slime
      :hook
      ((lisp-mode-hook . slime-mode)
       (lisp-mode-hook . slime-autodoc-mode))
      :custom ((inferior-lisp-program . "ros run"))
      :config
      (slime-setup '(slime-repl slime-fancy slime-banner slime-autodoc))))

  (leaf* scheme
    :config
    (leaf geiser
      :config
      (leaf geiser-guile)))

  (leaf dired
    :ensure nil
    :defun
    (dired-various-sort-change
     dired-convert-coding-system)
    :preface
    (defun dired-open-in-accordance-with-situation ()
      (interactive)
      (cond
       ((string-match "\\(?:\\.\\.?\\)"
                      (format "%s" (thing-at-point 'filename)))
        (dired-find-alternate-file))
       ((file-directory-p (dired-get-filename))
        (dired-find-alternate-file))
       (t
        (dired-find-file))))
    (defvar dired-various-sort-type
      '(("S" . "size")
        ("X" . "extension")
        ("v" . "version")
        ("t" . "date")
        ("" . "name")))
    (defun dired-various-sort-change (sort-type-alist &optional prior-pair)
      (when (eq major-mode 'dired-mode)
        (let* (case-fold-search
               get-next
               (options (mapconcat 'car sort-type-alist ""))
               (opt-desc-pair
                (or prior-pair
                    (catch 'found
                      (dolist (pair sort-type-alist)
                        (when get-next (throw 'found pair))
                        (setq get-next
                              (string-match (car pair) dired-actual-switches)))
                      (car sort-type-alist)))))
          (setq dired-actual-switches
                (concat "-l"
                        (replace-regexp-in-string
                         (concat "[l" options "-]") ""
                         dired-actual-switches)
                        (car opt-desc-pair)))
          (setq mode-name (concat "Dired by " (cdr opt-desc-pair)))
          (force-mode-line-update)
          (revert-buffer))))
    (defun dired-various-sort-change-or-edit (&optional arg)
      "Hehe"
      (interactive "P")
      (when dired-sort-inhibit (error "Cannot sort this dired buffer"))
      (if arg
          (dired-sort-other (read-string "ls switches (must contain -l): "
                                         dired-actual-switches))
        (dired-various-sort-change dired-various-sort-type)))
    :bind
    ((dired-mode-map
      ("w"   . wdired-change-to-wdired-mode)
      ("e"   . wdired-change-to-wdired-mode)
      ("s"   . dired-various-sort-change-or-edit)
      ("TAB" . dired-hide-details-mode)))
    :commands
    (dired-find-alternate-file
     dired-get-filename
     dired-log
     dired-make-relative
     dired-map-over-marks-check
     dired-get-marked-files
     dired-sort-other)
    :custom ((ls-lisp-dirs-first . t))
    :config
    (put 'dired-find-alternate-file 'disabled nil)

    (defvar dired-default-file-coding-system nil
      "*Default coding system for converting file (s).")
    (defvar dired-file-coding-system 'no-conversion)

    (defun dired-convert-coding-system ()
      (let ((file (dired-get-filename))
            (coding-system-for-write dired-file-coding-system)
            failure)
        (condition-case err
            (with-temp-buffer
              (insert-file-contents file)
              (write-region (point-min) (point-max) file))
          (error (setq failure err)))
        (if (not failure)
            nil
          (dired-log "convert coding system error for %s:\n%s\n" file failure)
          (dired-make-relative file))))

    (defun dired-do-convert-coding-system (coding-system &optional arg)
      "Convert file (s) in specified coding system."
      (interactive
       (list (let ((default (or dired-default-file-coding-system
                                buffer-file-coding-system)))
               (read-coding-system
                (format "Coding system for converting file (s) (default, %s): "
                        default)
                default))
             current-prefix-arg))
      (check-coding-system coding-system)
      (setq dired-file-coding-system coding-system)
      (dired-map-over-marks-check
       (function dired-convert-coding-system) arg 'convert-coding-system t))
    ;;   )
    (defun my-dired-process-marked-file (fn)
      "Run function FN on each file marked in dired."
      (interactive)
      (let* ( (marked-files (dired-get-marked-files)) )
        (mapc (lambda (filename)
                (let (mybuffer)
                  (find-file filename)
                  ;;process
                  (funcall fn)
                  ;;exit
                  (save-buffer)
                  (kill-buffer mybuffer)))
              marked-files)))

    (leaf dired-aux
      :ensure nil
      :bind
      ((:dired-mode-map
        ("T" . dired-do-convert-coding-system))))

    (leaf dired-filter
      :after dired
      (define-key dired-mode-map (kbd "/") dired-filter-map))
    (leaf diredfl
      :global-minor-mode
      diredfl-global-mode)
    (leaf dired-git-info
      :bind
      (dired-mode-map
       ("G" . dired-git-info-mode))))

  (leaf dockerfile-mode)

  (leaf ess
    :config
    (leaf ess-r-mode
      :ensure nil
      :commands ess-r-mode
      :defvar ess-r-mode-map
      :config
      (leaf ess-R-data-view
        :mykie
        (:ess-r-mode-map
         :package ess-r-mode
         ("C-c v" :default ess-R-dv-ctable :C-u ess-R-dv-pprint)))
      (leaf inlineR
        :bind
        ((:ess-r-mode-map
          :package ess-r-mode
          ("C-c i" . inlineR-insert-tag)))
        :defvar inlineR-re-funcname
        :config
        (setq inlineR-re-funcname (concat inlineR-re-funcname "\\|gg.*")))
      (leaf uimage
        :bind
        ((:ess-r-mode-map
          :package ess-r-mode
          ("C-c C-i" . uimage-mode)))))
    (leaf e2wm-R
      :config
      (defun ess-create-temp-buffer (buffer-name)
        (generate-new-buffer buffer-name))))

  (leaf org
    :ensure t org-contrib
    :commands (org-capture
               org-metaright
               org-metaleft
               org-metadown
               org-metaup
               org-meta-return
               org-cycle)
    :defun org-up-element org-element-at-point org-element-property
    :custom
    `((org-agenda-sticky . t)
      (org-directory . my-org-directory)
      (org-log-done . 'time)
      (org-default-notes-file . ,(expand-file-name "inbox.org" my-org-directory))
      (org-agenda-files . '(,my-org-directory))
      (org-todo-keywords
       . '((sequence "TODO(t)" "WAIT(w)" "|" "OBSOLETED(o)" "DONE(d)")))
      (org-capture-templates
       . '(("n" "Note" entry (file org-default-notes-file)
            "** %?\n   %i\n   %a\n   %U")
           ("t" "Todo" entry (file org-default-notes-file)
            "** TODO %?\n   %i\n   %a\n   %U")
           ("b" "Bug"  entry (file org-default-notes-file)
            "** TODO :bug: %?\n   %i\n   %a\n   %U")
           ("m" "Medicine notebook" entry
            (file+headline ,(expand-file-name "medicine.org" my-org-directory)
                           "notebook")
            "** %u
   disease   :: %?
   hospital  ::
   doctor    ::
   pharmacy  ::
   pharmacist::
   symptom   ::

")
           ("D" "Blog Diary" entry
            (file ,(expand-file-name "org/diary.org" my-blog-directory))
            "\
* TODO %?
  :PROPERTIES:
  :EXPORT_FILE_NAME: %(format \"%s-%s\" (format-time-string \"%Y\") (uuid-string))
  :END:
")
           ("I" "Blog Information Science" entry
            (file ,(expand-file-name "org/information-science.org" my-blog-directory))
            "\
* TODO %?
  :PROPERTIES:
  :EXPORT_FILE_NAME: %(format \"%s-%s\" (format-time-string \"%Y\") (uuid-string))
  :END:
")
           ("B" "Blog Medicine/Biology" entry
            (file ,(expand-file-name "org/medicine-biology.org" my-blog-directory))
            "\
* TODO %?
  :PROPERTIES:
  :EXPORT_FILE_NAME: %(format \"%s-%s\" (format-time-string \"%Y\") (uuid-string))
  :END:
")
           ("P" "Blog Physics" entry
            (file ,(expand-file-name "org/physics.org" my-blog-directory))
            "\
* TODO %?
  :PROPERTIES:
  :EXPORT_FILE_NAME: %(format \"%s-%s\" (format-time-string \"%Y\") (uuid-string))
  :END:
")
           ("M" "Blog Mathematics" entry
            (file ,(expand-file-name "org/mathematics.org" my-blog-directory))
            "\
* TODO %?
  :PROPERTIES:
  :EXPORT_FILE_NAME: %(format \"%s-%s\" (format-time-string \"%Y\") (uuid-string))
  :END:
")
           ))
      (org-blackfriday--org-element-string
       . '((table . "Table")
           (figure . "Figure")
           (src-block . "Code"))))

    ;; Replace "@&" with "&" in .org file when export to HTML.
    ;;  You can write "@&#x7c;" if you'd like to write "|".
    :preface
    (defun my:org-export-html-hook ()
      (perform-replace "@&amp;" "&" nil nil nil))
    (defun my:org-save-all-org-buffers (&rest _)
      (org-save-all-org-buffers))
    :hook
    ((org-export-html-final-hook . my:org-export-html-hook)
     (org-trigger-hook . my:org-save-all-org-buffers)
     (org-clock-in-hook . org-save-all-org-buffers)
     (org-clock-out-hook . org-save-all-org-buffers)
     (org-clock-cancel-hook . org-save-all-org-buffers))
    :bind
    (("M-q" . org-capture)
     (:org-mode-map
      ("C-M-f" . org-metaright)
      ("C-M-b" . org-metaleft)
      ("C-M-p" . org-metadown)
      ("C-M-n" . org-metaup)
      ("C-M-<non-convert>" . org-meta-return)
      ("<windows>" . org-cycle)))
    :init
    (leaf org-agenda
      :ensure nil
      :bind
      (("C-c a" . org-agenda))
      :custom
      (org-agenda-prefix-format
       . '((agenda . " %i %-12:c%?-12t% s %(let ((state (ignore-errors (org-get-todo-state)))) (if state (propertize state 'face (org-get-todo-face state)) \"\")) %b")
           (todo . " %i %-12:c %T  %(let ((state (ignore-errors (org-get-todo-state)))) (if state (propertize state 'face (org-get-todo-face state)) \"\")) %b")
           (tags . " %i %-12:c %T")
           (search . " %i %-12:c")))
      :preface
      (defun my:org-agenda-color-todo-state (&optional _)
        (remove-overlays)
        (let ((start 0) ov)
          (mapc
           (lambda (arg)
             (let ((regexp (car arg))
                   (face (cdr arg)))
               (save-excursion
                 (goto-char (point-min))
                 (while (string-match regexp (buffer-string) start)
                   (setq ov (make-overlay (1+ (match-beginning 0)) (1+ (match-end 0))))
                   (overlay-put ov 'face face)
                   (setq start (match-end 0)))
                 (setq start 0))))
           '(("\\_<\\(TODO\\|WAIT\\)\\_>" . org-todo)
             ("\\_<\\(DONE\\|OBSOLETED\\)\\_>" . org-done)))
          (save-excursion
            (goto-char (point-min))
            (while (string-match
                    "\\(TODO\\|WAIT\\|DONE\\|OBSOLETED\\)[^\n]*?\\(\\1 \\)[^\n]*$"
                    (buffer-string) start)
              (setq ov (make-overlay (1+ (match-beginning 2)) (1+ (match-end 2))))
              (overlay-put ov 'display "")
              (setq start (match-end 0)))
            (setq start 0))))
      :hook
      ((org-agenda-finalize-hook . my:org-agenda-color-todo-state))
      :advice
      (:after org-agenda-todo my:org-agenda-color-todo-state))

    (leaf org-readme
      :ensure nil
      :el-get
      emacsmirror:yaoddmuse emacsmirror:header2 emacsmirror:lib-requires
      vapniks/org-readme
      :depends http-post-simple)

    (leaf org-appear
      :hook
      ((org-mode-hook . org-appear-mode))
      :custom
      ((org-appear-autolinks . t)
       (org-appear-autosubmarkers . t)
       (org-appear-autoentities . t)
       (org-appear-autokeywords . t)
       (org-appear-inside-latex . t)))

    :config
    (defun my-org-netlify (str)
      (concat
       (save-restriction
         (widen)
         (save-excursion
           (while (ignore-errors (org-up-element) t))
           (let* ((entry (org-element-at-point)))
             (org-element-property :EXPORT_FILE_NAME entry))))
       "/" str))

    (leaf org-eldoc
      :ensure nil
      :commands (org-eldoc-get-breadcrumb
                 org-eldoc-get-src-header
                 org-eldoc-get-src-lang)
      :hook
      (org-mode-hook . eldoc-mode))

    (leaf org-id
      :ensure nil
      :custom
      `((org-id-locations-file
         . ,(expand-file-name
             "etc/.org-id-locations"
             user-emacs-directory))))

    (leaf gcal
      :ensure nil
      :el-get misohena/gcal
      :defvar (gcal-access-token)
      :defer t
      :custom
      `((gcal-token-file . ,(expand-file-name "etc/.gcal-token"
                                              user-emacs-directory))
        (gcal-org-pushed-events-file
         . ,(expand-file-name ".gcal-org-pushed-events"
                              my-org-directory))
        (gcal-org-header-separator . "->")
        (gcal-org-include-parents-header-maximum . t)
        (gcal-org-allowed-timestamp-prefix . '(nil "SCHEDULED" "DEADLINE"))
        (gcal-org-summary-prefix-ts-prefix-alist
         . '(("SCHEDULED" . "S:")
             ("DEADLINE" . "D:")
             (nil . ""))))
      :preface
      (defvar my:gcal-mailaddress)
      :pl-post-custom
      (gcal-client-id
       gcal-client-secret
       my:gcal-mailaddress)
      :init
      (leaf gcal-org
        :ensure nil
        :commands
        (gcal-org-push-file
         my:gcal-org-push-all-file
         my:gcal-org-push-file)
        :preface
        (defun my:gcal-org-push-all-file ()
          (interactive)
          (mapcar
           (apply-partially #'gcal-org-push-file my:gcal-mailaddress)
           (cl-remove-if-not
            (apply-partially #'string-match "/[^./][^/]*\\.org$")
            (directory-files my-org-directory t))))
        (defun my:gcal-org-push-file (file)
          (interactive
           (list
            (read-file-name
             "Org file: " my-org-directory nil t nil
             (apply-partially #'string-match "^[^\\./][^/]*\\.org$"))))
          (condition-case nil
              (gcal-org-push-file my:gcal-mailaddress file)
            (error
             (setq gcal-access-token nil)
             (gcal-org-push-file my:gcal-mailaddress file))))))

    (leaf ox-hugo
      :after org
      :custom
      ((org-hugo-auto-set-lastmod . t)
       (org-hugo-link-desc-insert-type . t))))

  (leaf* TeX
    :config
    (defun replace-to-comma ()
      (interactive)
      (save-excursion
        (goto-char (point-min))
        (while (search-forward "、")
          (replace-match  ", "))))
    (defun to-comma-hook ()
      (add-hook 'before-save-hook 'replace-to-comma nil t))

    (leaf tex-mode
      :defer t
      :custom
      ((tex-command . "platex --synctex=1")
       (tex-run-command . "platex")
       (latex-run-command . "platex")))

    (leaf bibtex
      :defer t
      :custom
      (bibtex-command . "pbibtex"))

    (leaf yatex
      :mode ("\\.tex\\'" . yatex-mode)
      :defun
      YaTeX-typeset-menu
      :preface
      (defun ad:yatex-typeseting-sentinel (_ arg)
        (when (string= arg "finished\n")
          (ignore-errors
            (with-current-buffer
                (if (string-match-p "\\.pdf$" (buffer-file-name))
                    (current-buffer)
                  (find-buffer-visiting
                   (YaTeX-get-preview-file-name "dvipdfmx")))
              (revert-buffer nil t)))))
      (defun YaTeX-typeset-pdf (arg)
        (interactive "P")
        (YaTeX-typeset-menu arg ?d))
      :advice
      (:after YaTeX-typeset-sentinel ad:yatex-typeseting-sentinel)
      :bind ((:YaTeX-prefix-map
              ("v" . yatex-open-dvi-with-emacs))
             (:YaTeX-mode-map
              ("C-c C-c" . YaTeX-typeset-pdf)))
      :custom
      `((YaTeX-user-completion-table
         . ,(expand-file-name "etc/.yatexrc" user-emacs-directory))
        ;; https://oku.edu.mie-u.ac.jp/~okumura/texfaq/qa/52930.html
        (YaTeX-latex-message-code . 'utf-8))
      :defun
      YaTeX-get-preview-file-name
      :config
      (defun yatex-open-dvi-with-emacs ()
        "Preview current dvi file."
        (interactive)
        (find-file-other-window
         (YaTeX-get-preview-file-name))))

    (leaf latex-math-preview
      :defun YaTeX-in-math-mode-p
      :bind (:YaTeX-prefix-map
             :package yatex
             ("p" . latex-math-preview-expression))
      :custom
      (latex-math-preview-in-math-mode-p-func . #'YaTeX-in-math-mode-p)
      (latex-math-preview-tex-to-png-for-preview . '(platex dvipng))
      (latex-math-preview-image-foreground-color . "black")
      (latex-math-preview-image-background-color . "white"))

    (leaf magic-latex-buffer
      :hook (latex-mode-hook . magic-latex-buffer)
      :custom
      ((magic-latex-enable-block-highlight . t)
       (magic-latex-enable-suscript        . t)
       (magic-latex-enable-pretty-symbols  . t)
       (magic-latex-enable-block-align     . nil)
       (magic-latex-enable-inline-image    . t)
       (magic-latex-enable-minibuffer-echo . nil))
      :defvar ml/arrow-symbols
      :config
      (add-to-list 'ml/arrow-symbols '("\\\\Longrightarrow\\>" . "⇒"))
      (add-to-list 'ml/arrow-symbols '("\\\\Longlefttarrow\\>" . "⇐"))
      (add-to-list 'ml/arrow-symbols '("\\\\Longleftrightarrow\\>" . "⇔"))
      (add-to-list 'ml/arrow-symbols '("\\\\longmapsto\\>" . "↦")))
    ;; C-c v open pdf
    ;; C-c p view math directly

    ;; (leaf xenops
    ;;   :hook (yatex-mode-hook . xenops-mode))

    (leaf reftex
      :hook
      ((latex-mode-hook . turn-on-reftex)
       (yatex-mode-hook . turn-on-reftex))
      :require reftex-ref
      :ensure auctex
      :defun
      (reftex-access-scan-info
       reftex-offer-label-menu)
      :custom
      `((reftex-default-bibliography
         . ',(list (expand-file-name
                    "texmf/bibtex/bib/mine.bib"
                    (pcase system-type
                      (`windows-nt (getenv "USERPROFILE"))
                      (_ (getenv "HOME")))))))
      :preface
      (defun my:eqref ()
        (interactive)
        (require 'tex)
        (reftex-access-scan-info current-prefix-arg)
        (insert
         (format "\\eref{%s}" (nth 0 (car (reftex-offer-label-menu "e"))))))
      (defun my:tabref ()
        (interactive)
        (require 'tex)
        (reftex-access-scan-info current-prefix-arg)
        (insert
         (format "\\tabref{%s}" (nth 0 (car (reftex-offer-label-menu "t"))))))
      (defun my:figref ()
        (interactive)
        (require 'tex)
        (reftex-access-scan-info current-prefix-arg)
        (insert
         (format "\\figref{%s}" (nth 0 (car (reftex-offer-label-menu "f"))))))
      (defun my:secref ()
        (interactive)
        (require 'tex)
        (reftex-access-scan-info current-prefix-arg)
        (insert
         (format "\\ref{%s}" (nth 0 (car (reftex-offer-label-menu "s"))))))
      :hydra
      (hydra-yatex-ref
       (:color blue)
       "\\ref"
       ("e" my:eqref "equation")
       ("t" my:tabref "table")
       ("f" my:figref "figure")
       ("s" my:secref "section"))
      :bind
      (:reftex-mode-map
       ("C-c C-r" . hydra-yatex-ref/body))))

  (leaf* pdf
    :config
    (leaf doc-view
      :ensure nil
      :defer t
      :preface
      (defun ad:doc-view-revert-buffer (orig &rest args)
        (let ((undo-outer-limit 0))
          (apply orig args)))
      :advice
      (:around doc-view-revert-buffer ad:doc-view-revert-buffer)
      :hook (doc-view-mode . buffer-disable-undo))
    (leaf pdf-tools
      :if (eq system-type 'gnu/linux)
      :mode ("\\.pdf\\'" . pdf-view-mode)))

  (leaf* web
    :config
    (leaf prettier
      :global-minor-mode
      global-prettier-mode)

    (leaf eslintd-fix
      :hook
      ((web-mode . eslintd-fix-mode)))

    (leaf web-mode
      :mode (("\\.phtml\\'"     . web-mode)
             ("\\.tpl\\.php\\'" . web-mode)
             ("\\.[agj]sp\\'"   . web-mode)
             ("\\.as[cp]x\\'"   . web-mode)
             ("\\.erb\\'"       . web-mode)
             ("\\.mustache\\'"  . web-mode)
             ("\\.djhtml\\'"    . web-mode)
             ("\\.html?\\'"     . web-mode))
      :custom
      (web-mode-enable-auto-closing . t)
      (web-mode-enable-auto-pairing . t)
      (web-mode-enable-block-face .   t)
      (web-mode-enable-part-face .    t)
      (web-mode-enable-current-column-highlight . t)
      (web-mode-code-indent-offset . 2)
      (web-mode-markup-indent-offset . 2))

    (leaf js
      :custom
      ((js-indent-level . 2)))

    (leaf typescript-mode
      :mode
      (("\\.tsx\\'" . typescript-mode)))

    (leaf css-mode
      :custom
      ((css-indent-offset . 2))))

  (leaf elisp-mode
    :ensure nil
    :defer t
    :config
    (leaf edit-list
      :commands edit-list)
    (leaf* test
      :config
      (leaf undercover)
      (leaf cursor-test))
    :bind
    ("C-<f1>" . elisp-slime-nav-describe-elisp-thing-at-point)
    :mykie
    ("<f12>" :default eval-buffer :region eval-region))

  (leaf lisp-mode
    :ensure nil
    :preface
    (defun my:byte-compile-this ()
      "byte-compile opened file."
      (interactive)
      (byte-compile-file (buffer-file-name)))
    :defun
    byte-compile-file
    :bind
    (:emacs-lisp-mode-map
     ("C-c c" . my:byte-compile-this)
     ("C-c l" . toggle-let-astah)
     ("M-d"  . edebug-defun))
    (:lisp-interaction-mode-map
     ("M-d" . edebug-defun)))

  (leaf clojure-mode)

  (leaf rust-mode)

  (leaf visual-basic-mode
    :ensure nil
    :el-get emacsmirror:visual-basic-mode)

  (leaf yaml-mode
    :mode
    (("\\.clang-format" . yaml-mode)))

  (leaf qml-mode
    :mode
    (("\\.qbs\\'" . qml-mode)))

  (leaf keg-mode)

  (leaf* gdb
    :custom
    (gud-gdb-command-name . "gdb"))

  (leaf* shell
    :config
    (leaf coterm
      :global-minor-mode
      coterm-mode)

    (leaf comint
      :ensure nil
      :custom
      (comint-scroll-show-maximum-output . t)
      :custom-face
      (comint-highlight-prompt '((t (:foreground "#00ff00")))))

    (leaf ansi-color
      :ensure nil
      :defun
      ansi-color-apply-on-region
      :defvar
      compilation-filter-start
      :config
      ;; https://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
      (defun my-colorize-compilation-buffer ()
        (when (provided-mode-derived-p major-mode 'compilation-mode)
          (let ((inhibit-read-only t))
            (ansi-color-apply-on-region compilation-filter-start (point-max)))))
      :hook
      (compilation-filter-hook . my-colorize-compilation-buffer))

    (leaf ccc
      :commands ccc-set-buffer-local-cursor-color)
    (defun hook:shell// ()
      ""
      (face-remap-set-base 'default :background "black" :foreground "gray")
      (ccc-set-buffer-local-cursor-color "gray")
      ;; (ccc-set-buffer-local-background-color "white")
      (display-line-numbers-mode -1))
    (defalias 'hook:eshell// 'hook:shell//)

    (leaf eshell
      :custom
      `(eshell-directory-name
        . ,(expand-file-name
            "etc/eshell"
            user-emacs-directory))
      :custom-face
      ((eshell-prompt . '((t (:foreground "#00ff00")))))
      :defun
      eshell-printn
      :config
      (defun eshell/e (arg)
        (eshell-printn
         (pp-to-string (eval (if (listp arg) arg (read (format "%s" arg))))))
        nil)
      :hook
      (eshell-mode-hook . hook:eshell//))

    (leaf shell
      :preface
      (defun hook:shell-mode-coding ()
        (pcase system-type
          (`windows-nt
           (set-process-coding-system (get-buffer-process (current-buffer)) 'utf-8-dos 'utf-8-dos))
          (_ (set-process-coding-system (get-buffer-process (current-buffer)) 'utf-8-unix 'utf-8-unix))))
      :hook
      ((shell-mode-hook . hook:shell-mode-coding)
       (shell-mode-hook . hook:eshell//))))

  (leaf cmake-mode)

  (leaf newsticker
    :custom
    `(newsticker-dir
      . ,(expand-file-name
          "etc/newsticker/"
          user-emacs-directory)))

  (leaf ahk-mode
    :preface
    (defun hook:ahk-mode-set-buffer-coding-system ()
      (setq buffer-file-coding-system 'utf-8-with-signature))
    :hook (ahk-mode-hook . hook:ahk-mode-set-buffer-coding-system))

  (leaf* communication-tool
    :config
    (leaf twittering-mode
      :commands (twittering-pop)
      :defun (twittering-get-buffer-list twittering-mode)
      :custom
      `((twittering-use-master-password . t)
        (twittering-private-info-file
         . ,(expand-file-name "etc/twittering-mode.gpg" user-emacs-directory))
        (twittering-account-authorization . 'authorized)
        (twittering-status-format
         . ,(concat
             "%FACE[my:for-gray-face]{%RT{   \0xf079 Retweeted by %S\n"
             "}}%i %S %FACE[my:for-deepgray-face]{@%s     %@ }\n %T \n"
             "%FACE[my:for-gray-face]{%FACE[bold]{"
             "\0xf079 %FIELD[%s]{retweet_count}  ♡ %FIELD[%s]{favorite_count}}}\n"
             " -------------------------------------------")))
      :custom-face
      ((twittering-username-face . '((t (:bold t))))
       (twittering-uri-face . '((t ("DeepSkyBlue3")))))
      :preface
      (defface my:for-gray-face
        '((t (:foreground "gray50")))
        "")
      (defface my:for-deepgray-face
        '((t (:foreground "gray40")))
        "")
      :hook
      (twittering-mode-hook . twittering-icon-mode)
      :bind
      (:twittering-mode-map
       ("<" . my-beginning-of-buffer)
       (">" . my-end-of-buffer)
       ("F" . twittering-favorite))
      :config
      (defun twittering-pop ()
        (interactive)
        (if (twittering-get-buffer-list)
            (display-buffer ":home")
          (let ((cb (buffer-name)))
            (twittering-mode)
            (switch-to-buffer cb)
            (display-buffer ":home")))))

    (leaf wanderlust
      :config
      (leaf wl                         ;Wanderlust (mailer)
        :ensure nil
        :custom
        `((wl-init-file
           . ,(expand-file-name "etc/.wl" user-emacs-directory))
          (wl-folders-file
           . ,(expand-file-name "etc/.folders" user-emacs-directory))
          (elmo-imap4-default-authenticate-type . 'clear))))))

(leaf editorconfig
  :global-minor-mode editorconfig-mode)

(leaf* semantic-helper
  :config
  (leaf eldoc
    :ensure nil
    :defun
    (eldoc--supported-p
     eldoc-print-current-symbol-info
     eldoc-mode
     eldoc-docstring-format-sym-doc
     my:eldoc-schedule-timer-timer)
    :hook
    (emacs-lisp-mode-hook . eldoc-mode)
    :preface
    ;; Stop eldoc to use labmda expression as timer.
    (defun my:eldoc-schedule-timer-timer ()
      (when (or eldoc-mode
                (and global-eldoc-mode
                     (eldoc--supported-p)))
        (eldoc-print-current-symbol-info)))
    (defun my:eldoc-schedule-timer ()
      "Ensure `eldoc-timer' is running.

If the user has changed `eldoc-idle-delay', update the timer to
reflect the change."
      (or (and eldoc-timer
               (memq eldoc-timer timer-idle-list)) ;FIXME: Why?
          (setq eldoc-timer
                (run-with-idle-timer
                 eldoc-idle-delay nil
                 #'my:eldoc-schedule-timer-timer))))
    :advice
    ((:override eldoc-schedule-timer my:eldoc-schedule-timer))
    :custom
    (eldoc-idle-delay . 1)
    (eldoc-echo-area-use-multiline-p . t))

  (leaf flycheck
    :defun (global-flycheck-mode flycheck-error-message)
    :global-minor-mode global-flycheck-mode
    :custom
    (((flycheck-display-errors-function
       . 'flycheck-display-error-messages))
     (flycheck-idle-change-delay . 2)
     (flycheck-check-syntax-automatically
      . '(save idle-change new-line mode-enabled))
     (flycheck-disabled-checkers
      ;; too late
      . '(emacs-lisp-checkdoc
          ;; no include file
          c/c++-clang))))

  (leaf flyspell
    :mykie (("<f7>" :default flyspell-buffer :region flyspell-region))
    :init
    (leaf ispell                     ;spell check
      :custom
      (ispell-program-name . "hunspell")
      :config
      (add-to-list
       'ispell-skip-region-alist
       '("\"\\([^\000-\377]\\|[ \n\t.,()0-9@:;/\\\\`{}*+<>?_]\\)+\"")))))

(leaf* input-method
  :config
  (leaf consult
    :bind
    (("C-s" . consult-line)
     ("C-x b" . consult-buffer)
     ("M-y" . consult-yank-from-kill-ring)
     ("<help> a" . consult-apropos)
     ("M-g g" . consult-goto-line)
     ("M-g o" . consult-outline)
     ("M-g i" . consult-imenu)
     ("M-g I" . consult-imenu-multi)
     ("M-g m" . consult-mark)
     ("M-g M" . consult-global-mark)
     ("M-s f" . consult-find)
     ("M-s k" . consult-focus-lines)
     ("M-@" . consult-register-load)
     ("C-M-@" . consult-register-store)
     ("M-g h" . consult-org-heading)
     ("M-g a" . consult-org-agenda))
    :defvar
    consult-buffer-sources
    :config
    (add-to-list 'consult-buffer-sources 'rhq-consult-source-project-directory))

  (leaf consult-ag
      :bind
      (("C-r" . consult-ag)))


  (leaf consult-flycheck
    :bind
    (("M-g e" . consult-flycheck)))

  (leaf embark
    :bind
    (("C-." . embark-act)
     ("M-." . embark-dwim)
     ("<help> B" . embark-bindings))
    :defvar
    (embark-file-map
     embark-buffer-map
     embark-bookmark-map)
    :config
    ;; https://karthinks.com/software/fifteen-ways-to-use-embark/
    (eval-when-compile
      (defmacro my/embark-ace-action (fn)
        `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
           (interactive)
           (with-demoted-errors "%s"
             (require 'ace-window)
             (let ((aw-dispatch-always t))
               (aw-switch-to-window (aw-select nil))
               (call-interactively (symbol-function ',fn)))))))

    (define-key embark-file-map (kbd "o") (my/embark-ace-action find-file))
    (define-key embark-buffer-map (kbd "o") (my/embark-ace-action switch-to-buffer))
    (define-key embark-bookmark-map (kbd "o") (my/embark-ace-action bookmark-jump))

    (eval-when-compile
      (defmacro my/embark-split-action (fn split-type)
        `(defun ,(intern (concat "my/embark-"
                                 (symbol-name fn)
                                 "-"
                                 (car (last  (split-string
                                              (symbol-name split-type) "-"))))) ()
           (interactive)
           (funcall #',split-type)
           (call-interactively #',fn))))

    (define-key embark-file-map     (kbd "2") (my/embark-split-action find-file split-window-below))
    (define-key embark-buffer-map   (kbd "2") (my/embark-split-action switch-to-buffer split-window-below))
    (define-key embark-bookmark-map (kbd "2") (my/embark-split-action bookmark-jump split-window-below))

    (define-key embark-file-map     (kbd "3") (my/embark-split-action find-file split-window-right))
    (define-key embark-buffer-map   (kbd "3") (my/embark-split-action switch-to-buffer split-window-right))
    (define-key embark-bookmark-map (kbd "3") (my/embark-split-action bookmark-jump split-window-right))

    (defun sudo-find-file (file)
      "Open FILE as root."
      (interactive "FOpen file as root: ")
      (when (file-writable-p file)
        (user-error "File is user writeable, aborting sudo"))
      (find-file (if (file-remote-p file)
                     (concat "/" (file-remote-p file 'method) ":"
                             (file-remote-p file 'user) "@" (file-remote-p file 'host)
                             "|sudo:root@"
                             (file-remote-p file 'host) ":" (file-remote-p file 'localname))
                   (concat "/sudo:root@localhost:" file))))

    (define-key embark-file-map (kbd "S") 'sudo-find-file))

  (leaf embark-consult
    :hook
    (embark-collect-mode-hook . consult-preview-at-point-mode))

  (leaf marginalia
    :global-minor-mode marginalia-mode)

  (leaf vertico
    :global-minor-mode vertico-mode
    :custom
    ((vertico-count . 30)))

  (leaf orderless
    :require t
    :custom
    ((completion-styles . '(orderless basic))
     (completion-category-overrides . '((file (styles orderless-migemo basic partial-completion))
                                       (consult-location (styles orderless-migemo basic partial-completion)))))
    :config
    ;; 1 character migemo regexp is too long
    (defun migemo-get-pattern-3 (word)
      (and (< 3 (length word))
           (migemo-get-pattern word)))

    (orderless-define-completion-style orderless-migemo
      (orderless-matching-styles '(orderless-literal
                                   orderless-regexp
                                   migemo-get-pattern-3))))

  (leaf corfu
    :global-minor-mode global-corfu-mode
    :custom
    (corfu-auto . t)
    :config
    (leaf corfu-doc
      :hook
      (corfu-mode-hook . corfu-doc-mode))
    (leaf kind-icon
      :custom
      (kind-icon-default-face . 'corfu-default)))

  (leaf cape
    :custom
    `((cape-dict-file
       . ,(cl-some
           (lambda (arg)
             (when (file-exists-p arg)
               (expand-file-name arg)))
           '("/etc/dictionaries-common/words"
             "~/.guix-profile/share/web2"))))
    :push
    ((completion-at-point-functions . #'cape-file)
     (completion-at-point-functions . #'cape-keyword)
     (completion-at-point-functions . #'cape-dict))))

(leaf* restores
  :custom
  (history-length . t)
  :config
  ;; Save history even when abort on minibuffer.
  (defadvice abort-recursive-edit
      (before minibuffer-save activate)
    (when (eq (selected-window) (active-minibuffer-window))
      (add-to-history minibuffer-history-variable (minibuffer-contents))))

  (leaf* auto-save
    :custom
    ;; auto-save file
    `((delete-auto-save-files . t)
      (auto-save-file-name-transforms
       . ',`((".*" ,(expand-file-name "etc/backup/" user-emacs-directory) t)))
      (auto-save-list-file-prefix
       . ,(expand-file-name
           "etc/auto-save-list/.saves-"
           user-emacs-directory))
      (auto-save-timeout . 10)
      (auto-save-interval . 500)
      ;; backup file
      (make-backup-files . t)
      (backup-directory-alist
       . ',`((".*" . ,(expand-file-name
                       "etc/backup"
                       user-emacs-directory))))
      (version-control .     t)
      (kept-new-versions .   5)
      (kept-old-versions .   1)
      (delete-old-versions . t)))

  (leaf savehist
    :ensure nil
    :custom
    `((savehist-file
       . ,(expand-file-name
           "etc/history"
           user-emacs-directory)))
    :global-minor-mode
    savehist-mode)

  (leaf savekill
    :custom
    `((save-kill-file-name
       . ,(expand-file-name "etc/kill-ring-saved.el" user-emacs-directory))
      (savekill-max-saved-items . nil)))

  (leaf recentf
    :require t
    :defvar
    recentf-exclude
    :custom
    `((recentf-max-saved-items . 200)
      (recentf-auto-cleanup . 'never)
      (recentf-save-file
       . ,(expand-file-name
           "etc/recentf"
           user-emacs-directory)))
    :global-minor-mode
    recentf-mode
    :config
    (add-to-list 'recentf-exclude ".*\\.emacs\\.d/elpa/.*"))

  (leaf saveplace
    :custom
    `((save-place . t)
      (save-place-file
       . ,(expand-file-name "etc/.emacs-places" user-emacs-directory)))))

(leaf* management
  :config
  (leaf rhq
    :require t
    :bind
    (("C-x C-p" . rhq-open-project-or-clone))
    :defvar
    rhq-consult-source-project-directory)

  (leaf* git
    :config
    (leaf transient
      :emacs>= "25.1"
      :custom
      `((transient-history-file
         . ,(expand-file-name
             "etc/transient/history.el"
             user-emacs-directory))
        (transient-levels-file
         . ,(expand-file-name
             "etc/transient/levels.el"
             user-emacs-directory))
        (transient-values-file
         . ,(expand-file-name
             "etc/transient/values.el"
             user-emacs-directory))))

    (leaf magit
      :emacs>= "25.1"
      :bind(("M-g M-g" . magit-status))
      :config
      (add-to-list 'process-coding-system-alist '("git" utf-8 . utf-8))
      (leaf git-commit
        :preface
        (defun my:set-git-coding-system ()
          (set-buffer-file-coding-system 'utf-8))
        :hook
        ((git-commit-mode-hook . my:set-git-coding-system)))
      (leaf forge
        :custom
        `((forge-database-file
           . ,(expand-file-name "etc/forge-database.sqlite"
                                user-emacs-directory)))))))

(leaf* move
  :bind
  (("C-a" . series-home)
   ("C-e" . series-end)
   (:prog-mode-map
    :package prog-mode
    ("C-a" . series-home-prog)
    ("C-e" . series-end-prog))
   (:eww-mode-map
    :package eww
    ("a" . series-home-prog)
    ("e" . series-end-prog))
   (:org-mode-map
    :package org
    ("C-a" . series-home-org)
    ("C-e" . series-end-org)))
  :config
  (defun forward-to-not-comment ()
    (interactive)
    (beginning-of-line)
    (forward-char (string-match
                   (concat "[ \t]*\\(\\(" (regexp-quote comment-start) ".*?$\\)\\|$\\)")
                   (buffer-substring (point-at-bol) (point-at-eol)))))

  (defun forward-to-not-whitespace ()
    (interactive)
    (beginning-of-line)
    (forward-char
     (string-match
      (concat "[ \t]*$") (buffer-substring (point-at-bol) (point-at-eol)))))

  (series-defun series-home
    back-to-indentation
    beginning-of-line
    beginning-of-buffer
    series-return)
  (series-defun series-end
    forward-to-not-whitespace
    end-of-line
    end-of-buffer
    series-return)
  (series-defun series-home-prog
    back-to-indentation
    beginning-of-line
    beginning-of-defun
    beginning-of-buffer
    series-return)
  (series-defun series-end-prog
    forward-to-not-comment
    end-of-line end-of-defun
    end-of-buffer
    series-defun)
  (series-defun series-home-org
    back-to-indentation
    org-beginning-of-line
    beginning-of-buffer
    series-return)
  (series-defun series-end-org
    org-end-of-line
    end-of-buffer
    series-return)


  (leaf bm
    :custom
    `((bm-repository-file
       . ,(expand-file-name "etc/.bm-repository" user-emacs-directory))
      (bm-buffer-persistence . t))
    :bind
    (("M-SPC" . bm-toggle)
     ("C-\\" . bm-toggle))
    :bind*
    (("C-M-n" . bm-next)
     ("C-M-p" . bm-previous))
    :custom-face
    (bm-persistent-face
     . '((((class grayscale) (background light)) (:background "DimGray"))
         (((class grayscale) (background dark))  (:background "LightGray"))
         (((class color)     (background light)) (:foreground nil :background "#090960"))
         (((class color)     (background dark))  (:foreground nil :background "#090960"))))
    :hook
    ((after-init-hook        . bm-repository-load)
     (find-file-hook         . bm-buffer-restore)
     (after-revert-hook      . bm-buffer-restore)
     (kill-buffer-hook       . bm-buffer-save)
     (after-save-hook        . bm-buffer-save)
     (vc-before-checkin-hook . bm-buffer-save)
     (kill-emacs-hook        . bm-buffer-save)
     (find-file-hook        .  bm-buffer-restore)
     (kill-buffer-hook       . bm-buffer-save)
     (after-save-hook        . bm-buffer-save)
     (after-revert-hook      . bm-buffer-restore)
     (vc-before-checkin-hook . bm-buffer-save)))

  (leaf* ace/avy
    :config
    (leaf ace-jump-mode
      :bind
      (("C-:" . ace-jump-char-mode))
      :custom
      `((ace-jump-mode-move-keys
         . ',(append "asdfghjkl;:qwertyuiopzxcvbnm,." nil)))
      :config
      (defun ace-jump-char-mode-migemo (query-char)
        "AceJump char mode"
        (interactive (list (read-char "Query Char:")))
        (if ace-jump-current-mode (ace-jump-done))

        (if (eq (ace-jump-char-category query-char) 'other)
            (error "[AceJump] Non-printable character"))

        (setq ace-jump-query-char query-char)
        (setq ace-jump-current-mode 'ace-jump-char-mode)
        (ace-jump-do
         (if (require 'migemo nil t)
             (let ((result (migemo-get-pattern (make-string 1 query-char))))
               (if (string-empty-p result)
                   (regexp-quote (make-string 1 query-char))
                 result))
           (regexp-quote (make-string 1 query-char)))))
      (advice-add #'ace-jump-char-mode :override #'ace-jump-char-mode-migemo))

    (leaf ace-jump-zap
      :bind (("M-z" . ace-jump-zap-to-char)))

    (leaf ace-link
      :bind
      (:Info-mode-map
       :package info
       (":" . ace-link-info))
      (:help-mode-map
       :package help-mode
       (":" . ace-link-help))
      (:eww-link-keymap
       :package eww
       (":" . ace-link-eww))
      (:eww-mode-map
       :package eww
       ( ":" . ace-link-eww))
      (:org-mode-map
       :package org
       ("C-c M-:" . ace-link-org))))

  (leaf* search
    :config
    (leaf ag
      :bind ("C-r" . ag-regexp)
      :custom
      ((ag-arguments . '("--smart-case" "--stats" "-z"))))))

(leaf* edit
  :preface
  (defun copy-all ()
    (interactive)
    (kill-ring-save (point-min) (point-max))
    (message "Copy all."))
  (defun comment-line ()
    "Make the line comment."
    (interactive)
    (save-excursion
      (beginning-of-line)
      (set-mark (point))
      (end-of-line)
      (comment-or-uncomment-region (region-beginning) (region-end))))
  :bind
  (("C-M-w" . copy-all))
  :mykie
  (:prog-mode-map
   ("M-;" :default comment-dwim :C-u comment-line))
  :config
  (define-key key-translation-map (kbd "C-h") (kbd "DEL"))

  (leaf browse-kill-ring)

  (leaf* parenthesis
    :config
    (leaf smartparens
      :require t
      :defun
      (sp--looking-back-p
       sp-local-pair)
      :bind
      ((smartparens-strict-mode-map
        ("C-S-h" . sp-splice-sexp)
        ("C-S-i" . sp-forward-slurp-sexp)
        ("C-S-o" . sp-forward-barf-sexp)
        ("M-I" . sp-backward-slurp-sexp)
        ("M-O" . sp-backward-barf-sexp)
        ("M-s r" . sp-rewrap-sexp)
        ("C-M-d" . sp-delete-symbol)))
      :global-minor-mode
      (show-smartparens-global-mode
       smartparens-global-strict-mode)
      :custom
      ((sp-highlight-pair-overlay . nil)
       (sp-base-key-bindings . 'sp))
      :config
      (require 'smartparens-config)

      ;; https://github.com/Fuco1/smartparens/issues/823
      ;; Allow ES6 arrow '=>' in react-mode
      (defun sp-after-equals-p (_id action _context)
        (when (memq action '(insert navigate))
          (sp--looking-back-p "=>" 2)))

      (defun sp-after-equals-skip (ms mb _me)
        (when (string= ms ">")
          (save-excursion
            (goto-char mb)
            (sp--looking-back-p "=" 1))))

      (sp-local-pair '(web-mode react-mode) "<" nil
                     :unless '(:add sp-after-equals-p)
                     :skip-match 'sp-after-equals-skip))

    (leaf rainbow-delimiters
      :hook ((lisp-mode-hook       . rainbow-delimiters-mode)
             (emacs-lisp-mode-hook . rainbow-delimiters-mode)
             (c-mode-common-hook   . rainbow-delimiters-mode))
      :custom-face
      (rainbow-delimiters-unmatched-face
       . '((t (:foreground "#ff0000" :bold t :underline t))))
      (rainbow-delimiters-depth-1-face . '((t (:foreground "#bbffff"))))
      (rainbow-delimiters-depth-2-face . '((t (:foreground "#ffbbff"))))
      (rainbow-delimiters-depth-3-face . '((t (:foreground "#ffffaa"))))
      (rainbow-delimiters-depth-4-face . '((t (:foreground "#aaddaa"))))
      (rainbow-delimiters-depth-5-face . '((t (:foreground "#ff55ff"))))
      (rainbow-delimiters-depth-6-face . '((t (:foreground "#09d999"))))
      (rainbow-delimiters-depth-7-face . '((t (:foreground "#ff6666"))))
      (rainbow-delimiters-depth-8-face . '((t (:foreground "#66ff66"))))
      (rainbow-delimiters-depth-9-face . '((t (:foreground "#6666ff"))))))

  (leaf smart-newline
    :global-minor-mode
    :hook
    ((prog-mode-hook           . smart-newline-mode)
     (text-mode-hook           . smart-newline-mode)
     (makefile-mode-hook       . smart-newline-mode-off)
     (makefile-gmake-mode-hook . smart-newline-mode-off)
     (yaml-mode-hook           . smart-newline-mode-off))
    :preface
    (defun smart-newline-mode-off ()
      (smart-newline-mode -1)))

  (leaf electric-operator
    :hook ((c-mode-common-hook . electric-operator-mode)
           (typescript-mode-hook . electric-operator-mode)
           (ess-r-mode-hook . electric-operator-mode)))

  (leaf align
    :ensure nil
    :bind
    ("C-x a" . align-regexp))

  (leaf expand-region
    :bind
    (:region-bindings-mode-map
     :package region-bindings-mode
     ("M-SPC" . er/contract-region)
     ("C-SPC" . er/expand-region)))

  (leaf visual-regexp
    :bind (("C-c r" . vr/query-replace)))

  (leaf grugru
    :custom
    `((grugru-edit-save-file . ,(expand-file-name ".grugru" user-emacs-directory)))
    :bind
    (("C-;" . grugru)
     ("<C-tab>" . grugru))
    :require t
    :global-minor-mode grugru-highlight-mode
    :config
    (grugru-default-setup)
    (grugru-edit-load))

  (leaf* undo
    :custom
    ((undo-outer-limit . nil))
    :config
    (leaf winner
      :ensure nil
      :commands
      (winner-mode
       winner-undo
       winner-redo)
      :global-minor-mode
      winner-mode)

    (leaf undo-tree
      :commands undo-tree-mode
      :defvar undo-tree-mode
      :custom
      ((undo-tree-auto-save-history . nil))
      :hydra
      (hydra-undo
       (:hint nil :color pink :foreign-keys 'nil
              :pre (unless undo-tree-mode (undo-tree-mode)))
       "
   ^Undo^         ^Winner^
^^^^^^^^------------------------------------
   _u_,_C-/_: undo      _<left>_:  undo

   _r_: redo          ^^_<right>_: redo

"
       ("u" undo-tree-undo)
       ("C-/" undo-tree-undo)
       ("M-u" undo-tree-undo)
       ("r" undo-tree-redo)
       ("q" nil "quit")
       ("t" undo-tree-visualize "visualize tree" :exit t)
       ("<left>" winner-undo)
       ("<right>" winner-redo))
      :bind
      ((("C-x u" . hydra-undo/undo-tree-undo)
        ("M-u" . hydra-undo/undo-tree-undo)
        (:undo-tree-map
         ("C-x u" . hydra-undo/undo-tree-undo)
         ("C-/" . hydra-undo/undo-tree-undo))))
      :global-minor-mode
      global-undo-tree-mode)))

(leaf* help
  :bind
  (("<f1> h" . nil))
  :custom-face
  (help-argument-name . '((t (:bold t :italic t))))
  :config
  (leaf find-func
    :ensure nil
    :custom
    `((find-function-C-source-directory
       . ,(expand-file-name
           "../src" (invocation-directory)))))

  (leaf helpful
    :bind
    (("<help> k" . helpful-key)
     ("<help> f" . helpful-callable)
     ("<help> v" . helpful-variable)))

  (leaf which-key
    :emacs>= "24.4"
    :require t
    :global-minor-mode
    which-key-mode
    :config
    (which-key-setup-side-window-bottom)
    :custom
    (which-key-idle-delay . 1)))

(leaf info
  :ensure nil
  :init
  (straight-use-package
   '(emacs-ja
     :host github
     :repo "ayatakesi/ayatakesi.github.io"
     :files ("emacs/26.1/emacs-ja.info")))

  (straight-use-package
   '(org-ja
     :host github
     :repo "org-mode-doc-ja/org-ja"
     :files ("docs/org-ja")))

  (straight-use-package
   '(yatex
     :host github
     :repo "emacsmirror/yatex"
     :files
     ("docs/*")))
  (straight-use-package
   '(magit-ja
     :host github
     :repo "kuma35/magit-docs-ja"
     :files ("Documentation/docs-ja/*")))

  :defer t
  :preface
  ;; http://rubikitch.com/2016/07/06/emacs245-manual-ja/
  (defun Info-find-node--info-ja (orig-fn filename &rest args)
    (apply orig-fn
           (pcase filename
             ("emacs" "emacs245-ja")
             (_ filename))
           args))
  (defun Info-find-node--info-ja (orig-fn filename &rest args)
    (apply orig-fn
           (pcase filename
             ("emacs" "emacs-ja")
             (_ filename))
           args))
  :advice
  (:around Info-find-node Info-find-node--info-ja)
  :config
  (add-to-list 'Info-directory-list "~/info/")
  (add-to-list 'Info-directory-list
               (expand-file-name "info/" user-emacs-directory))
  (add-to-list 'Info-directory-list
               (expand-file-name "../share/info/" invocation-directory)))

(leaf google-translate
  :defun
  google-translate-translate
  :preface
  (defvar google-translate-english-chars "[:ascii:]"
    "Regexp which matches English")
  (defun google-translate-enja-or-jaen (&optional string)
    "Translate region or sentence by Google."
    (interactive)
    (setq string
          (cond ((stringp string) string)
                (current-prefix-arg
                 (read-string "Google Translate: "))
                ((use-region-p)
                 (buffer-substring (region-beginning) (region-end)))
                (t
                 (save-excursion
                   (let (s)
                     (forward-char 1)
                     (backward-sentence)
                     (setq s (point))
                     (forward-sentence)
                     (buffer-substring s (point)))))))
    (let* ((asciip (string-match
                    (format "\\`[%s]+\\'" google-translate-english-chars)
                    string)))
      (run-at-time 0.1 nil #'deactivate-mark)
      (google-translate-translate
       (if asciip "en" "ja")
       (if asciip "ja" "en")
       string)))
  :bind
  ("C-c t" . google-translate-enja-or-jaen))

(leaf* japanese
  :config
  (set-language-environment "Japanese")

  (leaf kkc
    :ensure nil
    :custom
    `((kkc-init-file-name
       . ,(expand-file-name
           "etc/kkcrc" user-emacs-directory))))

  (eval-when-compile
    (defvar my:jisyo-path (expand-file-name "dicts" user-emacs-directory)))
  (leaf ddskk
    :config
    (leaf skk
      :ensure nil
      :bind* (("C-j" . skk-mode))
      :bind
      (:skk-j-mode-map
       ("\\" . self-insert-command)
       ("$"  . self-insert-command))
      :defvar skk-mode skk-rom-kana-rule-list
      :preface
      (defun skk-yatex-hook ()
        (make-local-variable 'skk-j-mode-map)
        (define-key skk-j-mode-map "$" 'YaTeX-insert-dollar))
      :custom
      `(
        ;; Jisyo
        (skk-jisyo-code . 'utf-16le-with-signature-dos)
        (skk-inhibit-ja-dic-search . t)
        (skk-large-jisyo
         . ',(if (locate-file "skk-dict.txt" (list my:jisyo-path))
                 `(,(locate-file "skk-dict.txt" (list my:jisyo-path))
                   . sjis)
               (cons (expand-file-name "SKK-JISYO.L" my:jisyo-path) 'euc-jp)))
        (skk-extra-jisyo-file-list
         . ',(append
              (mapcar
               (lambda (arg) nil nil
                 (let
                     ((file (expand-file-name
                             (if (consp arg) (car arg) arg) my:jisyo-path))
                      (coding-system (cdr-safe arg)))
                   (if (file-exists-p file)
                       (if coding-system
                           (list (cons file coding-system))
                         (list file))
                     nil)))
               '(("skk-kana.txt" . sjis)
                 ("ddskk_medical_dic.txt" . euc-jp)))))
        (skk-itaiji-jisyo
         . ',(cons (expand-file-name "SKK-JISYO.itaiji" my:jisyo-path) 'euc-jp))
        (skk-jisyo
         . ,(expand-file-name "etc/.skk-jisyo" user-emacs-directory))
        ;; annotation
        (skk-show-annotation . t)
        (skk-annotation-delay . 3)
        ;; Azik
        (skk-use-azik . t)
        ;; Candidates
        (skk-show-inline . 'vertical)
        (skk-show-candidates-always-pop-to-buffer . nil)
        ;; Dynamic complete
        (skk-dcomp-activate . t)
        (skk-dcomp-multiple-activate . t)
        ;; Input/Output
        (skk-egg-like-newline . t)
        (skk-henkan-strict-okuri-precedence . t)
        (skk-sticky-key . ,(kbd
                            (pcase system-type
                              (`windows-nt "<non-convert>")
                              (_ "<muhenkan>"))))
        (skk-search-katakana . t)
        (skk-japanese-message-and-error . t)
        )
      :custom-face
      (
       (skk-dcomp-multiple-face
        . '((t (:foreground "black" :background "gray" :bold nil))))
       (skk-dcomp-multiple-trailing-face
        . '((t (:foreground "white" :bold nil))))
       (skk-dcomp-multiple-selected-face
        . '((t (:foreground "white" :background "steel blue" :bold nil))))

       (skk-dcomp-face . '((t (:foreground "#dfdfdf")))))
      :hook
      ((skk-mode-hook . skk-yatex-hook))
      :config
      ;; Download Jisyo
      (unless (file-exists-p my:jisyo-path)
        (skk-get my:jisyo-path))

      (add-to-list 'skk-rom-kana-rule-list '("z:" nil (":" . ":")) t)))

  (leaf jaword
    :global-minor-mode
    global-jaword-mode)

  (leaf migemo
    :defun migemo-init
    :require t
    :custom
    `((migemo-command . "cmigemo")
      (migemo-options . '("-q" "--emacs"))
      ;; Set your installed path
      (migemo-dictionary
       . ,(pcase system-type
            (`windows-nt
             (expand-file-name
              "dict/utf-8/migemo-dict"
              (file-name-directory (locate-file "cmigemo.exe" exec-path))))
            (_ (cl-some (lambda (arg) (when (file-exists-p arg) (expand-file-name arg)))
                        '("/usr/share/cmigemo/utf-8/migemo-dict"
                          "~/.guix-profile/share/migemo/utf-8/migemo-dict")))))
      (migemo-user-dictionary . nil)
      (migemo-regex-dictionary . nil)
      (migemo-coding-system . 'utf-8))
    :config
    (migemo-init)))

(leaf open-junk-file
  :bind(("C-x s" . open-junk-file))
  :custom
  `(open-junk-file-format
    . ,(expand-file-name
        "junk/%Y-%m-%d-%H%M%S."
        (getenv "HOME"))))

(leaf* competitive-programming
  :config
  (leaf online-judge
    :ensure nil
    :el-get ROCKTAKEY/emacs-online-judge
    :custom
    `((online-judge-directories
       . `,(list
            (expand-file-name "~/../../atcoder"))))
    :require t)
  (leaf oj
    :custom
    `((oj-home-dir . ,(expand-file-name "oj" "~"))
      (oj-default-online-judge . 'atcoder))
    :preface
    (define-key global-map (kbd "C-x j") (make-sparse-keymap))
    :bind
    (("C-x j t" . oj-test)
     ("C-x j s" . oj-submit)
     ("C-x j n" . oj-next)
     ("C-x j p" . oj-prev)
     ("C-x j P" . oj-prepare))))

(leaf* remote
  :config
  (leaf ssh
    :custom
    (ssh-program . "plink")
    (tramp-default-method . "psftp")
    :hook
    ((ssh-mode-hook . ssh-directory-tracking-mode)
     (ssh-mode-hook . shell-dirtrack-mode))))

(leaf* wsl
  ;; https://www.reddit.com/r/emacs/comments/6xryqh/emacs_in_wsl_and_the_windows_clipboard/
  :mykie
  (("C-y" :default yank :C-u wsl-paste))
  :preface
  (defun wsl-paste ()
    (interactive)
    (let ((wslbuffername "wsl-temp-buffer"))
      (get-buffer-create wslbuffername)
      (with-current-buffer wslbuffername
        (insert (let ((coding-system-for-read 'dos))
                  ;; TODO: put stderr somewhere else
                  (shell-command "powershell.exe -command 'Get-Clipboard' 2> /dev/null" wslbuffername nil)))
        (goto-char (point-min))
        (ignore-errors (delete-char 1)))
      (insert-buffer-substring wslbuffername)
      (kill-buffer wslbuffername)))

  (defun wsl-copy (start end)
    (interactive "r")
    (shell-command-on-region start end "clip.exe")
    (deactivate-mark)))

(leaf yasnippet
  :defvar
  (yas-new-snippet-default
   yas-snippet-dirs
   yas-maybe-expand)
  :defun
  (yas-load-directory
   yas-expand-snippet)
  :require t
  :bind
  (:my-yas-prefix-map
   ("n" . yas-new-snippet)
   ("v" . yas-visit-snippet-file)
   ("e" . yas-visit-snippet-file)
   ("i" . yas-insert-snippet))
  (:yas-minor-mode-map
   ("<deletechar>" . yas-skip-and-clear-or-delete-char)
   ("<tab>" . nil))
  :custom
  ;; expand yasnippet only beginning of symbol.
  (yas-key-syntaxes . '("w_"))
  (yas-also-auto-indent-first-line . t)
  :bind
  (:yas-minor-mode-map
   ("TAB" . nil))
  :mode
  ("snippets/" . snippet-mode)
  :defun
  (yas-snippet-from-region
   yas--guess-snippet-directories
   yas--table-mode)
  :preface
  (defvar my-yas-prefix-map (make-sparse-keymap) "Prefix keymap of C-xy")

  (defvar yas-original-buffer nil)
  (defun yas-snippet-from-region ()
    "Initial snippet content from region."
    (or (with-current-buffer yas-original-buffer
          (if (region-active-p)
              (replace-regexp-in-string
               "[\\$]" "\\\\\\&"
               (buffer-substring-no-properties
                (region-beginning) (region-end)))))
        ""))
  (defun yas-new-snippet--with-region (&optional no-template)
    "Pops a new buffer for writing a snippet.

Expands a snippet-writing snippet, unless the optional prefix arg
NO-TEMPLATE is non-nil."
    (interactive "P")
    (let ((guessed-directories (yas--guess-snippet-directories)))

      (setq yas-original-buffer (current-buffer))
      (switch-to-buffer "*new snippet*")
      (erase-buffer)
      (kill-all-local-variables)
      (snippet-mode)
      (yas-minor-mode 1)
      (set (make-local-variable 'yas--guessed-modes)
           (mapcar (lambda (d)
                     (yas--table-mode (car d)))
                   guessed-directories))
      (when (and (not no-template) yas-new-snippet-default)
        (save-excursion (insert (yas-snippet-from-region)))
        (yas-expand-snippet yas-new-snippet-default))))
  (defun hook:snippet/kill-whitespace-trancation ()
    (remove-hook 'before-save-hook #'delete-trailing-whitespace t))
  (defun my:initial-yas ()
    (setq yas-snippet-dirs
          (list
           (expand-file-name "snippets" user-emacs-directory)
           'yasnippet-snippets-dir))
    (yas-load-directory (expand-file-name "snippets" user-emacs-directory) t))
  :hook
  (after-init-hook . my:initial-yas)
  (snippet-mode-hook . hook:snippet/kill-whitespace-trancation)
  :advice
  (:override yas-new-snippet yas-new-snippet--with-region)
  :commands yas-new-snippet
  :global-minor-mode yas-global-mode
  :defvar yas-buffer-local-condition yas-not-string-or-comment-condition
  :config
  (define-key yas-minor-mode-map "\C-xy" my-yas-prefix-map)
  (setq yas-buffer-local-condition yas-not-string-or-comment-condition)

  (leaf yasnippet-snippets
    :require t
    :commands yasnippet-snippets-initialize
    :advice
    (:override yasnippet-snippets-initialize ignore))

  (leaf popup :require t)

  (defvar flymake-is-active-flag nil)
  (defadvice yas/expand-snippet
      (before inhibit-flymake-syntax-checking-while-expanding-snippet
              activate)
    (setq flymake-is-active-flag
    	  (or flymake-is-active-flag
    		  (assoc-default 'flymake-mode (buffer-local-variables))))
    (when flymake-is-active-flag
      (flymake-mode -1)))
  (add-hook 'yas-after-exit-snippet-hook
    		(lambda ()
              (when flymake-is-active-flag
                (flymake-mode 1) (setq flymake-is-active-flag nil))))

  (leaf auto-yasnippet
    :custom
    (aya-create-with-newline . t)
    :mykie
    ("C-x C-y" :default aya-expand :C-u! aya-create)
    ("C-x y y" :default aya-expand :C-u! aya-create))

  (define-key yas-minor-mode-map (kbd "SPC") yas-maybe-expand))

(leaf* autoinsert
  :require t
  :commands auto-insert-mode
  :custom
  `(auto-insert-directory
    . ,(expand-file-name "auto-insert" user-emacs-directory))
  :defvar auto-insert-alist
  :config
  (add-to-list
   'auto-insert-alist
   `(("\\(README\\)\\|\\([Rr]eadme\\)\\.org$"
      . "Readme.org header")
     "Short description: "
     "[[https://github.com/ROCKTAKEY/" (file-name-base (directory-file-name (project-root (project-current))))
     "][https://img.shields.io/github/tag/ROCKTAKEY/"
     (file-name-base (directory-file-name (project-root (project-current))))
     ".svg?style=flat-square]]\n"
     "[[file:LICENSE][https://img.shields.io/github/license/ROCKTAKEY/"
     (file-name-base (directory-file-name (project-root (project-current)))) ".svg?style=flat-square]]\n"
     "[[https://codecov.io/gh/ROCKTAKEY/" (file-name-base (directory-file-name (project-root (project-current))))
     "?branch=master][https://img.shields.io/codecov/c/github/ROCKTAKEY/"
     (file-name-base (directory-file-name (project-root (project-current)))) ".svg?style=flat-square]]\n"
     (if (string-match "^docker-" (file-name-base (directory-file-name (project-root (project-current)))))
         (let ((s (substring (file-name-base (directory-file-name (project-root (project-current)))) 7)))
           (concat
            "[[https://cloud.docker.com/repository/docker/rocktakey/"
            s
            "/][https://img.shields.io/docker/automated/rocktakey/"
            s
            ".svg?style=flat-square]]\n"))
       (concat
        "[[https://github.com/ROCKTAKEY/" (file-name-base (directory-file-name (project-root (project-current))))
        "/actions][https://img.shields.io/github/workflow/status/ROCKTAKEY/"
        (file-name-base (directory-file-name (project-root (project-current))))
        "/CI/master.svg?style=flat-square]]\n"
        ))
     "* " str "\n"
     _ "\n"
     "* How to Use?\n"
     "* License\n"
     "  This package is licensed by GPLv3. See [[file:LICENSE][LICENSE]].\n"
     ))

  (add-to-list 'auto-insert-alist
               '(("\\.el\\'" . "Emacs Lisp header")
                 "Short description: "
                 ";;; " (file-name-nondirectory (buffer-file-name)) " --- " str
                 (make-string (max 2 (- 80 (current-column) 27)) ?\s)
                 "-*- lexical-binding: t; -*-" '(setq lexical-binding t)
                 "

;; Copyright (C) " (format-time-string "%Y") "  "
                 (getenv "ORGANIZATION") | (progn user-full-name) "

;; Author: " (user-full-name)
                 '(if (search-backward "&" (line-beginning-position) t)
                      (replace-match (capitalize (user-login-name)) t t))
                 '(end-of-line 1) " <" (progn user-mail-address) ">
;; Keywords: "
                 '(require 'finder)
                 ;;'(setq v1 (apply 'vector (mapcar 'car finder-known-keywords)))
                 '(setq v1 (mapcar (lambda (x) (list (symbol-name (car x))))
		                           finder-known-keywords)
	                    v2 (mapconcat (lambda (x) (format "%12s:  %s" (car x) (cdr x)))
	                                  finder-known-keywords
	                                  "\n"))
                 ((let ((minibuffer-help-form v2))
                    (completing-read "Keyword, C-h: " v1 nil t))
                  str ", ") & -2 "

\;; Version: 0.0.0
\;; Package-Requires: ((emacs \"24.1\"))
\;; URL: https://github.com/"(user-full-name) "/" (file-name-base (directory-file-name (project-root (project-current)))) _ "\n"
                  "\
\;; This program is free software; you can redistribute it and/or modify
\;; it under the terms of the GNU General Public License as published by
\;; the Free Software Foundation, either version 3 of the License, or
\;; (at your option) any later version.

\;; This program is distributed in the hope that it will be useful,
\;; but WITHOUT ANY WARRANTY; without even the implied warranty of
\;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
\;; GNU General Public License for more details.

\;; You should have received a copy of the GNU General Public License
\;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

\;;; Commentary:

\;; "  "

\;;; Code:\n"

'(when (string-match "test/.*\\.el$\\|^.*-test\\.el" (file-name-base))
   "\
(require 'undercover)
(undercover \"*.el\"
            (:report-file \"coverage-final.json\")
            (:send-report nil))\n\n\n")

"
\(provide '"
                 (file-name-base)
                 ")
\;;; " (file-name-nondirectory (buffer-file-name)) " ends here\n"))

  (add-to-list 'auto-insert-alist
               '(("\\.tex\\'" . "TeX header.")
                 nil
                 "\\documentclass{jsarticle}\n"
                 "\\usepackage[dvipdfmx]{graphicx}\n"
                 "\\usepackage{tikz}\n"
                 "\\usepackage{mhchem}\n"
                 "\\usepackage{chemfig}\n"
                 "\n"
                 "\\begin{document}\n\n"
                 _ "\n"
                 "\\end{document}\n"))

  (add-to-list 'auto-insert-alist (cons "/junk/" nil)))

(leaf* char-code
  :config
  (defalias 'mojibake 'revert-buffer-with-coding-system)
  (defalias 'mojibakejanai 'set-buffer-file-coding-system)

  (setq-default buffer-file-coding-system 'utf-8)
  (when (eq system-type 'gnu/linux)
    (setq default-file-name-coding-system 'utf-8)))

(leaf* visual
  :custom
  ((text-quoting-style . 'straight)
   (use-dialog-box . nil)
   (show-help-function . #'message)
   (echo-keystrokes . 0.01)
   )
  :config
  (leaf* highlight
    :config
    (leaf highlight-indent-guides
      :hook (yaml-mode-hook . highlight-indent-guides-mode)
      :custom
      ((highlight-indent-guides-method . 'fill)
       (highlight-indent-guides-auto-enabled . t)
       (highlight-indent-guides-responsive . t)
       (highlight-indent-guides-delay . 0.9)))

    (leaf highlight-defined
      :hook
      (emacs-lisp-mode-hook . highlight-defined-mode))

    (leaf volatile-highlights
      :global-minor-mode
      volatile-highlights-mode)

    (leaf whitespace
      :ensure nil
      :global-minor-mode global-whitespace-mode
      :custom-face
      (whitespace-space
       . '((t (:foreground "#ffb90f" :background nil :underline t :bold t))))
      (whitespace-tab
       . '((t (:foreground "#caff70" :background nil :underline t))))
      :custom
      (whitespace-style
       . '(face tabs tab-mark spaces space-mark newline newline-mark))
      (whitespace-space-regexp . "\\(\x3000+\\)")
      (whitespace-display-mappings
       . '((tab-mark   ?\t   [?\xBB ?\t])))
      :config
      (defun trailing-whitespace-off ()
        (interactive)
        (setq show-trailing-whitespace nil)))

    (leaf rainbow-mode
      :hook emacs-lisp-mode-hook)

    (leaf color-identifiers-mode
      :hook c-mode-common-hook
      :custom
      (color-identifiers:num-colors . 20)))

  (defalias 'messasge-box 'message)

  (leaf* font
    :custom
    (use-default-font-for-symbols . nil)
    :if (display-graphic-p)
    :config
    (leaf font-lock-studio
      :doc
      "Developer tools for font-lock.")

    (defvar my:font
      (cond
       ((font-info "Cica")
        "Cica")
       ((font-info "Courier New")
        "Courier New")
       (t (face-attribute 'default :family))))

    (set-fontset-font
     "fontset-standard"
     'unicode
     (font-spec :family my:font))

    (mapc
     (lambda (arg)
       (eval `(push '(font . "fontset-standard") ,arg)))
     '(initial-frame-alist
       default-frame-alist))

    (set-face-attribute 'variable-pitch nil :family 'unspecified)
    (set-face-attribute 'fixed-pitch nil :family 'unspecified)

    )

  (leaf my-modeline
    :ensure nil
    :require t
    :defvar
    my:color-view-mode-enabled
    my:color-modeline-background)

  (add-to-list 'default-frame-alist '(cursor-type . box))

  ;; theme load
  (leaf* theme
    :custom
    `((custom-theme-directory . ,(expand-file-name "themes/" user-emacs-directory)))
    :config
    ;; (load-theme 'my-dark-green t)
    (load-theme 'my-dark-cream t)))

(leaf* view
  :leaf-defer nil
  :bind
  (("<f9>" . view-mode)
   (:view-mode-map
    ("s" . swiper)
    ("f" . forward-word)
    ("b" . backward-word)
    ("n" . next-line)
    ("p".  previous-line)
    ("o" . other-buffer)
    ("g" . goto-line)
    ("t" . hs-toggle-hiding)
    ("H" . hs-hide-all)
    ("L" . recenter-top-bottom)
    (" " . scroll-up)
    ("M-SPC" . scroll-down)
    ("v" . scroll-up)
    ("c" . scroll-down)
    ("V" . scroll-other-window)
    ("C" . scroll-other-window-down)
    (";" . ace-jump-char-mode)
    ("[" . bm-previous)
    ("]" . bm-next)
    ("\\" . bm-toggle)
    ("E" . eval-last-sexp)
    ("h" . backward-word)
    ("j" . next-line)
    ("k" . previous-line)
    ("l" . forward-word)))
  :preface
  (defvar view-mode-off-hook nil)
  (defun my:view-mode-off-hook (&rest _)
    (run-hooks 'view-mode-off-hook))
  (defun my:view-color ()
    (if view-mode
        (face-remap-add-relative 'mode-line
                                 :background my:color-view-mode-enabled)
      (face-remap-add-relative 'mode-line
                               :background my:color-modeline-background)))
  :advice
  (:after view--disable my:view-mode-off-hook)
  :hook
  ((view-mode-hook .  my:view-color)
   (view-mode-off-hook . my:view-color))
  :config
  (leaf* hide/show
    :leaf-defer nil
    :hydra
    (hydra-h
     (:color blue :hint nil)
     "
  ^hide-line^            ^hide/show^
--^---------^------------^^^^^^^--------------------
  _m_: match             _.__/__\\_: toggle
  _u_: unmatch           ^^^^^ _H_ : hide all
  _s_: show all          ^^^^^ _S_ : show all
"
     ("q" nil "Quit")
     ;; hide line
     ("m" hide-lines-matching)
     ("u" hide-lines-not-matching)
     ("s" hide-lines-show-all)
     ;; hs-mode
     ("H" hs-hide-all)
     ("S" hs-show-all)
     ("." hs-toggle-hiding)
     ("/" hs-toggle-hiding)
     ("\\" hs-toggle-hiding))
    :bind
    (("M-h" . hydra-h/body))
    :config
    (leaf hide-lines
      :doc
      "Hide line which matches regexp.")
    (leaf hideshow
      :defun
      hs-hide-level
      :hook
      (prog-mode-hook . hs-minor-mode)
      :preface
      (defun my-hs-hide-level ()
        (interactive)
        (hs-hide-level 0))
      :bind
      ((:hs-minor-mode-map
        ("C-\\"  . hs-toggle-hiding)
        ("M-s s" . hs-show-all))
       ("M-<non-convert>" . hs-toggle-hiding))
      :mykie
      (:hs-minor-mode-map
       :package hideshow
       ("M-s h" :default my-hs-hide-level :C-u hs-hide-all)))))

(leaf* disable-disabled
  :config
  (mapc
   (lambda (arg)
     (put arg 'disabled nil))
   '(downcase-region
     narrow-to-region
     list-timers
     upcase-region)))
