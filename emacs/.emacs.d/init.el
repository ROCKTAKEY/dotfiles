;;; init.el --- My init file
;;; Commentary:
;; My init file
;;; Code:
(defvar my-profiler-on nil)
(when my-profiler-on
  (profiler-start 'cpu))

(defun my-standard-value (symbol)
  "Return standard value of SYMBOL."
  (eval (car (get symbol 'standard-value))))

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

(eval-and-compile
  (unless (package-installed-p 'mic)
    (package-refresh-contents)
    (package-install 'mic))
  (require 'mic)
  (require 'mic-filter)
  (require 'mic-deffilter)

  (mic-deffilter-t-to-name my-mic-filter-package-t-to-name :package)
  (mic-deffilter-nonlist-to-list my-mic-filter-package-nonlist-to-list :package)
  (mic-deffilter-const-append my-mic-filter-package-append-t :package '(t))
  (mic-deffilter-nonlist-to-list my-mic-filter-require-nonlist-to-list :require)
  (mic-deffilter-t-to-name my-mic-filter-require-t-to-name :require)
  (mic-deffilter-ignore my-mic-filter-ignore-docs :doc)

  (mic-defmic mmic* mic
    :filters
    '(my-mic-filter-package-nonlist-to-list
      my-mic-filter-package-t-to-name
      my-mic-filter-require-nonlist-to-list
      my-mic-filter-require-t-to-name
      my-mic-filter-ignore-docs
      mic-filter-hook-list-maybe
      mic-filter-define-key-general
      mic-filter-hydra
      mic-filter-pretty-hydra
      mic-filter-pretty-hydra+
      mic-filter-mode-hydra
      mic-filter-mykie))

  (mic-defmic mmic mic
    :filters
    '(my-mic-filter-package-nonlist-to-list
      my-mic-filter-package-append-t
      my-mic-filter-package-t-to-name
      my-mic-filter-require-nonlist-to-list
      my-mic-filter-require-t-to-name
      my-mic-filter-ignore-docs
      mic-filter-hook-list-maybe
      mic-filter-define-key-general
      mic-filter-hydra
      mic-filter-pretty-hydra
      mic-filter-pretty-hydra+
      mic-filter-mode-hydra
      mic-filter-mykie)))

(mmic* straight
  :eval-installation
  ((let ((bootstrap-file
          (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
         (bootstrap-version 3))
     (unless (file-exists-p bootstrap-file)
       (with-current-buffer
           (url-retrieve-synchronously
            "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
            'silent 'inhibit-cookies)
         (goto-char (point-max))
         (eval-print-last-sexp)))
     (load bootstrap-file nil 'nomessage)))
  :custom
  ((straight-vc-git-default-clone-depth . 1)))

(mmic el-get
  :custom ((el-get-git-shallow-clone . t)))

(mmic hydra)

(mmic pretty-hydra)

(mmic major-mode-hydra
  :define-key
  ((global-map
    ("M-q" . #'major-mode-hydra))))

(mmic general)



(defun get-wsl-user-directory ()
  "Get Windows home directory in WSL."
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

(defvar my-dropbox-directory
  (expand-file-name
   "Dropbox"
   (cond
    ((eq system-type 'windows-nt) (getenv "USERPROFILE"))
    ((eq system-type 'gnu/linux)
     (cond
      ((getenv "WSL_DISTRO_NAME")
       (get-wsl-user-directory))
      (t (getenv "HOME")))))))

(defvar my-org-directory
  (expand-file-name "memo" my-dropbox-directory))

(defvar my-blog-directory
  (expand-file-name "~/rhq/github.com/ROCKTAKEY/blog"))

(mmic* userinfo
  :custom
  ((user-mail-address . "rocktakey@gmail.com")))

(mmic* startup
  :custom
  ((user-full-name . "ROCKTAKEY")))

(defgroup mine nil "My group."
  :group 'lisp)



(mmic dash)

(mmic region-bindings-mode
  :autoload-noninteractive
  (region-bindings-mode-enable)
  :eval-after-others
  ((region-bindings-mode-enable)))

;; (add-hook 'switch-buffer-functions
;;           (lambda (prev cur)
;;             (message "%S -> %S" (buffer-name prev) (buffer-name cur))))
(mmic switch-buffer-functions
  :eval
  ((defvar switch-buffer-hook nil
     "Hook which called like `switch-buffer-functions'.
No arguments is passed.")
   (defun run-switch-buffer-hook (_ _)
     (run-hooks 'switch-buffer-hook)))
  :hook
  ((switch-buffer-functions . #'run-switch-buffer-hook)))

;; port:   pacage name
;; host:   url
;; user:   user id
;; secret: passward
(mmic epg
  :custom
  ((epg-gpg-program . "gpg")
   (epg-pinentry-mode . 'loopback)))

(mmic pinentry
  :eval
  ((pinentry-start)))

(mmic* auth-source
  :custom
  ((auth-sources
    . (list
       (expand-file-name "etc/authinfo.gpg"   user-emacs-directory)
       (expand-file-name "etc/authinfo.plist" user-emacs-directory)))))

(mmic epa)

(mmic* plstore
  :custom ((plstore-encoded . t))
  :eval
  ((add-to-list 'auto-mode-alist
                '("\\.plist\\'" . plstore-mode))))

(mmic uuid
  :autoload-noninteractive (uuid-string))

(mmic* url
  :declare-function (url-do-setup)
  :custom
  ((url-configuration-directory
    . (expand-file-name "etc/url/"  user-emacs-directory)))
  :eval
  ((defun my:url-retrieve-synchronously (url &optional silent inhibit-cookies timeout)
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
   (advice-add #'url-retrieve-synchronously :override #'my:url-retrieve-synchronously)
   (defun set-url-proxy (server-name)
     "set http proxy"
     (interactive "shttp proxy server: ")
     (setq url-proxy-services `(("http" . ,server-name)))
     (setenv "http_proxy" server-name))))

(mmic eww
  :define-key-after-load
  ((eww-mode-map
    ("u" . #'eww-copy-page-url)
    ("n" . #'next-line)
    ("p" . #'previous-line)))
  :autoload-interactive (eww-copy-page-url)
  :custom
  ((eww-search-prefix . "http://www.google.co.jp/search?q=")))

(mmic* delsel
  :eval
  ((delete-selection-mode)))

(mmic* simple
  :define-key
  ((global-map
    ("C-d" . #'delete-forward-char)
    ("C-x l" . #'toggle-truncate-lines)))
  :eval-before-all
  ((defun ad:transpose-chars (&optional arg)
     "Same as `transpose-chars' except always use 2 chars before cursor."
     (interactive "P")
     (and (null arg) (forward-char -1))
     (transpose-subr 'forward-char (prefix-numeric-value arg)))
   (advice-add #'transpose-chars :override #'ad:transpose-chars))
  :eval
  ((column-number-mode)
   (line-number-mode))
  :custom
  ((kill-whole-line . t)
   (set-mark-command-repeat-pop . t)
   (mark-ring-max . 50))
  :hook
  ((before-save-hook . #'delete-trailing-whitespace)))

(mmic* cus-edit
  :custom
  ((custom-file . (expand-file-name "etc/custom.el" user-emacs-directory))))

(mmic* load
  :custom
  ((load-prefer-newer . t)))

(mmic server
  :custom
  ;; emacsclient
  ((server-auth-dir
    . (expand-file-name "etc/server/" user-emacs-directory)))
  :hook
  ((after-init-hook . #'server-start)))

(mmic* files
  :custom
  ((confirm-kill-emacs . #'y-or-n-p)
   (require-final-newline . t)
   (view-read-only . t)))

(mmic* window
  :define-key-general
  ((override
    ("C-o" . #'other-window)))
  :define-key
  ((global-map
    ("C-x 9" . #'split-window-3)))
  :hydra
  ((hydra-window-resizer
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
    ("q" nil "quit")))
  :mykie
  ((global-map
    ("C-w" :default hydra-window-resizer/body :region kill-region)))
  :eval
  ((defun split-window-3 ()
     (interactive)
     (split-window-right)
     (split-window-right)
     (balance-windows))))

(mmic swap-buffers
  :declare-function
  (swap-buffers)
  :eval
  ((defun swap-buffers-keep-focus ()
     (interactive)
     (swap-buffers t)))
  :define-key
  ((global-map
    ("C-x w" . #'swap-buffers-keep-focus))))

(mmic ace-window
  :define-key
  ((global-map
    ("M-o" . #'ace-window)))
  :custom
  ((aw-keys . '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?\;))
   (aw-dispatch-always . t)
   (display-buffer-base-action
    . '((display-buffer-reuse-window
         ace-display-buffer)))
   (display-buffer-alist
    . `(("\\*help\\[R" (display-buffer-reuse-mode-window
                        ace-display-buffer)
         (reusable-frames . nil))
        ("\\*R" nil (reusable-frames . nil))
        ("\\*Help\\*" . ,display-buffer-fallback-action)
        ("\\*embark\\*" . ,display-buffer-fallback-action)
        ("\\*Embark Actions\\*" . ,display-buffer-fallback-action)
        ("\\*Org Select\\*" . ,display-buffer-fallback-action)
        ("\\*helm" . ,display-buffer-fallback-action)
        ("magit-diff:" nil
         (inhibit-same-window . t)))))
  :face
  ((aw-leading-char-face
    . ((t (:foreground "red" :height 10.0))))
   (aw-mode-line-face
    . ((t (:background "#006000" :foreground "white" :bold t)))))
  :eval-after-others ((ace-window-display-mode)))

(mmic* uniquify
  :custom
  ((uniquify-buffer-name-style . 'post-forward-angle-brackets)
   (uniquify-ignore-buffers-re . "*[^*]")))

(mmic keyfreq
  :custom
  ((keyfreq-file . (expand-file-name "etc/emacs.keyfreq" user-emacs-directory)))
  :eval
  ((keyfreq-mode)
   (keyfreq-autosave-mode)))

(mmic* auto-revert
  :eval
  ((global-auto-revert-mode)))

(mmic* sounds
  :custom
  ((ring-bell-function . #'ignore)))

(mmic* startup
  :custom
  ((inhibit-startup-message . t)
   (initial-scratch-message . "")))

(mmic* indent
  :custom
  ((tab-width . 4)
   (indent-tabs-mode . nil)))

(mmic* scroll
  :custom
  ((scroll-conservatively . 35)
   (scroll-margin . 0)
   (scroll-step . 1)))

(mmic* scroll-bar
  :eval
  ((scroll-bar-mode -1)))

(mmic* frame
  :custom
  ((blink-cursor-blinks . 0))
  :define-key
  ((global-map
    ;; `suspend-frame' should not be run easily
    ("C-z" . nil))))

(mmic* time
  :custom
  ((display-time-day-and-date . t)
   (display-time-format . "%Y-%m-%d %a %-H:%M")
   (display-time-default-load-average . nil)))

(mmic* emacs
  :custom
  ((system-time-locale . "C")))

(mmic* filelock
  :custom
  ((create-lockfiles . nil)))

(mmic* eval
  :custom
  ((max-lisp-eval-depth . 5000)
   (max-specpdl-size . 5000)))

(mmic package-lint)

(mmic package
  :define-key
  ((package-menu-mode-map
    ("R" . #'package-reinstall))
   (global-map
    ("C-x p l" . #'package-list-packages)
    ("C-x p r" . #'package-refresh-contents))))

(mmic async
  :eval
  ((defun my-async-variables-noprops (sequence)
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
          (t sequence))))
  :custom
  ((async-variables-noprops-function . #'my-async-variables-noprops)))

(mmic multi-compile
  :define-key-with-feature
  ((cc-mode
    (c-mode-map
     ("C-c c" . #'multi-compile-run))
    (c++-mode-map
     ("C-c c" . #'multi-compile-run)))
   (typescript-mode
    (typescript-mode-map
     ("C-c c" . #'multi-compile-run))))
  :custom
  ((multi-compile-history-file
    . (expand-file-name "etc/multi-compile.cache" user-emacs-directory))
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
  :hook
  ((before-multi-compile-hook . #'save-buffer))
  :defvar-noninitial
  (multi-compile-template)
  :eval
  ((defvar before-multi-compile-hook nil)
   (defun ad:multi-compile-run ()
     (run-hooks 'before-multi-compile-hook))
   (advice-add #'multi-compile-run :before #'ad:multi-compile-run))
  :eval-after-load
  ((add-to-list
    'multi-compile-template
    '("%proj" . (project-root (project-current))))))

(mmic imenu
  :custom
  ((imenu-auto-rescan . t)))

(mmic imenu-list
  :custom
  ((imenu-list-position . 'left))
  :face
  ((imenu-list-entry-face
    . ((t (:width condensed :height 120))))
   (imenu-list-entry-subalist-face-0
    . ((t (:width expanded :height 130))))))

(mmic repeep
  :define-key
  ((global-map
    ("C-=" . #'repeep)))
  :eval
  ((repeep-macro-mode)))

;; (leaf lsp-mode
;;   :emacs>=  "25.1"
;;   :hook ((c-mode-hook           . lsp)
;;          (c++-mode-hook         . lsp)
;;          (web-mode-hook         . lsp)
;;          (dockerfile-mode-hook  . lsp)
;;          (yaml-mode-hook        . lsp)
;;          (r-mode-hook           . lsp)
;;          (javascript-mode-hook  . lsp)
;;          (js-mode-hook          . lsp)
;;          (typescript-mode-hook  . lsp)
;;          (sh-mode-hook          . lsp)
;;          (common-lisp-mode-hook . lsp)
;;          (yatex-mode-hook       . lsp)
;;          (tex-mode-hook         . lsp)
;;          (latex-mode-hook       . lsp)
;;          (bibtex-mode-hook      . lsp)
;;          (java-mode-hook        . lsp)
;;          (clojure-mode-hook     . lsp)
;;          (rust-mode-hook        . lsp))
;;   :defun (lsp-register-client make-lsp-client lsp-stdio-connection)
;;   :defvar (lsp--formatting-indent-alist)
;;   :bind
;;   (lsp-mode-map
;;    ("M-r" . lsp-rename)
;;    ("M-c" . lsp-execute-code-action))
;;   :custom
;;   `((lsp-session-file
;;      . ,(expand-file-name
;;          "etc/.lsp-session-v1"
;;          user-emacs-directory))
;;     (lsp-log-io . nil)
;;     (lsp-log-max . nil))
;;   :preface
;;   ;; shut-up view-mode message on lsp-mode.
;;   (defun ad:view-lsp (orig &rest aaa)
;;     (if (string= (buffer-name) " *temp*")
;;         (shut-up
;;           (apply orig aaa))
;;       (apply orig aaa)))
;;   :advice (:around view-mode-enter ad:view-lsp)
;;   :config
;;   (add-to-list 'lsp--formatting-indent-alist '(web-mode . web-mode-code-indent-offset))

;;   (leaf lsp-ui
;;     :hook (lsp-mode-hook . lsp-ui-mode)
;;     :bind
;;     (:lsp-ui-mode-map
;;      ("M-d" . lsp-ui-doc-mode))
;;     :custom
;;     ((lsp-ui-sideline-ignore-duplicate . t)
;;      (lsp-ui-sideline-show-hover . t))
;;     :custom-face
;;     (lsp-ui-sideline-symbol-info
;;      . '((t (:foreground "#6060dd" :background nil)))))

;;   (leaf ccls
;;     :require t
;;     :after cc-mode)

;;   (leaf lsp-latex
;;     :require t)

;;   (leaf lsp-java
;;     :custom
;;     `((lsp-java-project-referenced-libraries
;;        . ["lib/**/*.jar" ,(expand-file-name "~/Fiji.app/jars/*.jar")])))

;;   (leaf dap-mode
;;     :custom
;;     `((dap-breakpoints-file
;;        . ,(expand-file-name "etc/.dap-breakpoints" user-emacs-directory)))))

(mmic eglot
  :hook-list-maybe
  (((c-mode-hook
     c++-mode-hook
     web-mode-hook
     dockerfile-mode-hook
     yaml-mode-hook
     r-mode-hook
     javascript-mode-hook
     js-mode-hook
     typescript-mode-hook
     sh-mode-hook
     common-lisp-mode-hook
     yatex-mode-hook
     tex-mode-hook
     latex-mode-hook
     bibtex-mode-hook
     java-mode-hook
     clojure-mode-hook
     rust-mode-hook
     cmake-mode-hook)
    . eglot-ensure))
  :define-key-after-load
  ((eglot-mode-map
    ("M-r" . #'eglot-rename)))
  :eval-after-load
  ((setcdr (assoc '(c++-mode c-mode) eglot-server-programs)
           (eglot-alternatives
            '("ccls" "clangd")))))

(mmic tree-sitter
  :package tree-sitter-langs
  :defvar-noninitial
  (tree-sitter-major-mode-language-alist)
  :hook ((tree-sitter-after-on-hook . #'tree-sitter-hl-mode))
  :eval
  ((global-tree-sitter-mode)
   (tree-sitter-require 'tsx)
   (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-mode . tsx))))

(mmic dumb-jump
  :eval
  ((add-hook 'xref-backend-functions #'dumb-jump-xref-activate 100)))

(mmic ibuffer
  :define-key
  ((global-map
    ("C-x C-b" . #'ibuffer)))
  :custom
  ((ibuffer-formats
    . '((mark  vc-status-mini " "
               (name 18 18 :left :elide)
               " "
               (size 9 -1 :right)
               "  "
               (mode 16 16 :left :elide)
               " "
               (vc-status 16 16 :left)
               " "
               filename-and-process)))))

(mmic ibuffer-vc
  :eval
  (
   ;; Sort by repository
   (defun my-ibuffer-hook ()
     (ibuffer-vc-set-filter-groups-by-vc-root)
     (unless (eq ibuffer-sorting-mode 'alphabetic)
       (ibuffer-do-sort-by-alphabetic))))
  :hook
  ((ibuffer-hook . #'my-ibuffer-hook)))

(mmic quickrun
  :eval
  ((quickrun-add-command "c++/g++/c++17"
     '((:command . "g++")
       (:exec    . ("%c -std=c++17 %o -o %e %s -g3 -O0"
                    "%e %a"))
       (:remove  . ("%e"))
       (:description . "Compile C++ file with g++ -std=c++17 and execute"))
     :default "c++")))

(mmic npm)

(mmic cask-mode)

(mmic csv-mode)

(mmic markdown-mode)

(mmic cc-mode
  :eval
  ((defun my:c-statement-cont (cons)
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
   (add-to-list 'auto-mode-alist '(".*/include/c\\+\\+/.*" . c++-mode))))

(mmic modern-cpp-font-lock
  :hook ((c++-mode-hook . #'modern-c++-font-lock-mode)))

(mmic clang-format+
  :hook
  ((c++-mode-hook . #'clang-format+-mode)))

(mmic slime
  :hook
  ((lisp-mode-hook . #'slime-mode)
   (lisp-mode-hook . #'slime-autodoc-mode))
  :custom ((inferior-lisp-program . "ros run"))
  :eval
  ((slime-setup '(slime-repl slime-fancy slime-banner slime-autodoc))))

(mmic geiser)

(mmic geiser-guile)

(mmic* dired
  :define-key
  ((dired-mode-map
    ("TAB" . #'dired-hide-details-mode)))
  :custom
  ((ls-lisp-dirs-first . t)
   (dired-dwim-target . t))
  :eval
  ((put 'dired-find-alternate-file 'disabled nil)))

(mmic dired-filter)

(mmic diredfl
  :eval
  ((diredfl-global-mode)))

(mmic dired-git-info
  :declare-function
  (dired-git-info-auto-enable)
  :hook
  ((dired-after-readin-hook . #'dired-git-info-auto-enable))
  :custom
  ((dgi-auto-hide-details-p . nil)))

(mmic dockerfile-mode)

(mmic org
  :package org-contrib
  :mode-hydra
  (( org-mode ()
     ("Export"
      (("C" org-commentary-update "Export to Commentary")))))

  :custom
  ((org-agenda-sticky . t)
   (org-directory . my-org-directory)
   (org-log-done . 'time)
   (org-default-notes-file . (expand-file-name "inbox.org" my-org-directory))
   (org-agenda-files . (list my-org-directory))
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
  :hook
  ((org-clock-in-hook . #'org-save-all-org-buffers)
   (org-clock-out-hook . #'org-save-all-org-buffers)
   (org-clock-cancel-hook . #'org-save-all-org-buffers))
  :define-key-after-load
  ((org-mode-map
    ("C-M-f" . #'org-metaright)
    ("C-M-b" . #'org-metaleft)
    ("C-M-p" . #'org-metadown)
    ("C-M-n" . #'org-metaup)
    ("C-M-<non-convert>" . #'org-meta-return)
    ("<windows>" . #'org-cycle)))
  )

(mmic* org-agenda
  :define-key
  ((global-map
    ("C-c a" . #'org-agenda)))
  :custom
  ((org-agenda-prefix-format
    . '((agenda . " %i %-12:c%?-12t% s %(let ((state (ignore-errors (org-get-todo-state)))) (if state (propertize state 'face (org-get-todo-face state)) \"\")) %b")
        (todo . " %i %-12:c %T  %(let ((state (ignore-errors (org-get-todo-state)))) (if state (propertize state 'face (org-get-todo-face state)) \"\")) %b")
        (tags . " %i %-12:c %T")
        (search . " %i %-12:c"))))
  :eval
  ((defun my:org-agenda-color-todo-state (&optional _)
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
        (setq start 0)))))
  :eval-after-load
  ((advice-add #'org-agenda-todo :after #'my:org-agenda-color-todo-state))
  :hook
  ((org-agenda-finalize-hook . #'my:org-agenda-color-todo-state)))

(mmic org-commentary
  :autoload-interactive
  (org-commentary-update))

(mmic org-appear
  :hook
  ((org-mode-hook . #'org-appear-mode))
  :custom
  ((org-appear-autolinks . t)
   (org-appear-autosubmarkers . t)
   (org-appear-autoentities . t)
   (org-appear-autokeywords . t)
   (org-appear-inside-latex . t)))

(defun my-org-netlify (str)
  (concat
   (save-restriction
     (widen)
     (save-excursion
       (while (ignore-errors (org-up-element) t))
       (let* ((entry (org-element-at-point)))
         (org-element-property :EXPORT_FILE_NAME entry))))
   "/" str))

(mmic* org-eldoc
  :hook
  ((org-mode-hook . #'eldoc-mode)))

(mmic* org-id
  :custom
  ((org-id-locations-file
    . (expand-file-name
       "etc/.org-id-locations"
       user-emacs-directory))))

(mmic ox-hugo
  :custom
  ((org-hugo-auto-set-lastmod . t)
   (org-hugo-link-desc-insert-type . t)))

(defun replace-to-comma ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "、")
      (replace-match  ", "))))

(defun to-comma-hook ()
  (add-hook 'before-save-hook #'replace-to-comma nil t))

(mmic tex-mode
  :custom
  ((tex-command . "platex --synctex=1")
   (tex-run-command . "platex")
   (latex-run-command . "platex")))

(mmic bibtex
  :custom
  ((bibtex-command . "pbibtex")))

(mmic yatex
  :declare-function
  (YaTeX-typeset-menu
   YaTeX-get-preview-file-name)
  :define-key-after-load
  ((YaTeX-prefix-map
    ("v" . #'yatex-open-dvi-with-emacs))
   (YaTeX-mode-map
    ("C-c C-c" . #'YaTeX-typeset-pdf)))
  :custom
  ((YaTeX-user-completion-table
    . (expand-file-name "etc/.yatexrc" user-emacs-directory))
   ;; https://oku.edu.mie-u.ac.jp/~okumura/texfaq/qa/52930.html
   (YaTeX-latex-message-code . 'utf-8))
  :eval
  ((add-to-list 'auto-mode-alist
                '("\\.tex\\'" . yatex-mode))
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
   (defun yatex-open-dvi-with-emacs ()
     "Preview current dvi file."
     (interactive)
     (find-file-other-window
      (YaTeX-get-preview-file-name))))
  :eval-after-load
  ((advice-add #'YaTeX-typeset-sentinel :after #'ad:yatex-typeseting-sentinel)))

(mmic auctex)

(mmic reftex
  :hook
  ((latex-mode-hook . #'turn-on-reftex)
   (yatex-mode-hook . #'turn-on-reftex))
  :declare-function
  (reftex-access-scan-info
   reftex-offer-label-menu)
  :custom
  ((reftex-default-bibliography
    . (list (expand-file-name
             "texmf/bibtex/bib/mine.bib"
             (pcase system-type
               (`windows-nt (getenv "USERPROFILE"))
               (_ (getenv "HOME")))))))
  :eval
  ((defmacro my-define-ref (name command label-menu)
     `(defun ,name ()
        (interactive)
        (require 'tex)
        (require 'reftex-ref)
        (reftex-access-scan-info current-prefix-arg)
        (insert
         (format ,(format "\\%s{%%s}" command) (nth 0 (car (reftex-offer-label-menu ,label-menu)))))))
   (my-define-ref my:eqref "eref" "e")
   (my-define-ref my:tabref "tabref" "t")
   (my-define-ref my:figref "figref" "f")
   (my-define-ref my:secref "ref" "s"))
  :hydra
  ((hydra-yatex-ref
    (:color blue)
    "\\ref"
    ("e" my:eqref "equation")
    ("t" my:tabref "table")
    ("f" my:figref "figure")
    ("s" my:secref "section")))
  :define-key-after-load
  ((reftex-mode-map
    ("C-c C-r" . #'hydra-yatex-ref/body))))

(when (eq system-type 'gnu/linux)
  (mmic pdf-tools
    :eval
    ((add-to-list 'auto-mode-alist
                  '("\\.pdf\\'" . pdf-view-mode)))))

(mmic eslintd-fix
  :hook
  ((web-mode-hook . #'eslintd-fix-mode)))

(mmic web-mode
  :eval
  ((mapc
    (apply-partially #'add-to-list 'auto-mode-alist)
    '(("\\.phtml\\'" . web-mode)
      ("\\.tpl\\.php\\'" . web-mode)
      ("\\.[agj]sp\\'" . web-mode)
      ("\\.as[cp]x\\'" . web-mode)
      ("\\.erb\\'" . web-mode)
      ("\\.mustache\\'" . web-mode)
      ("\\.djhtml\\'" . web-mode)
      ("\\.html?\\'" . web-mode))))
  :custom
  ((web-mode-enable-auto-closing . t)
   (web-mode-enable-auto-pairing . t)
   (web-mode-enable-block-face .   t)
   (web-mode-enable-part-face .    t)
   (web-mode-enable-current-column-highlight . t)
   (web-mode-code-indent-offset . 2)
   (web-mode-markup-indent-offset . 2)))

(mmic js
  :custom
  ((js-indent-level . 2)))

(mmic typescript-mode
  :eval
  ((add-to-list 'auto-mode-alist
                '("\\.tsx\\'" . typescript-mode))))

(mmic css-mode
  :custom
  ((css-indent-offset . 2)))

(mmic edit-list
  :autoload-interactive
  (edit-list))

(mmic undercover)

(mmic cursor-test)

(mmic* elisp-mode
  :mykie
  ((emacs-lisp-mode-map
    ("<f12>" :default eval-buffer :region eval-region)))
  :define-key-after-load
  ((emacs-lisp-mode-map
    ("C-c c" . #'my:byte-compile-this)))
  :mode-hydra
  (( emacs-lisp-mode (:title "Emacs Lisp Mode")
     ("Eval"
      (("b" eval-buffer "Eval Buffer")
       ("r" eval-buffer "Eval region"))
      "Edit"
      (("s" string-edit-at-point "Edit string")
       ("l" edit-list "Edit list"))
      "Test/Debug"
      (("t" ert "Run test")
       ("d" edebug-defun "Debug defun")
       ("e" macrostep-expand "Expand")
       ("p" package-lint-current-buffer "Lint")
       ("c" checkdoc "Checkdoc")))))
  :eval
  ((defun my:byte-compile-this ()
     "byte-compile opened file."
     (interactive)
     (byte-compile-file (buffer-file-name)))))

(mmic elisp-docstring-mode)

(mmic string-edit-at-point)

(mmic clojure-mode)

(mmic rust-mode)

(mmic yaml-mode
  :eval
  ((add-to-list 'auto-mode-alist
                '("\\.clang-format" . yaml-mode))))

(mmic qml-mode
  :eval
  ((add-to-list 'auto-mode-alist
                 '("\\.qbs\\'" . qml-mode))))

(mmic keg-mode)

(mmic gdb-mi
  :custom
  ((gud-gdb-command-name . "gdb")))

(mmic coterm
  :eval
  ((coterm-mode)))

(mmic* comint
  :custom
  ((comint-scroll-show-maximum-output . t))
  :face
  ((comint-highlight-prompt . ((t (:foreground "#00ff00"))))))

(mmic* ansi-color
  :hook
  ((compilation-filter-hook . #'my-colorize-compilation-buffer))
  :eval
  ;; https://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
  ((defun my-colorize-compilation-buffer ()
     (when (provided-mode-derived-p major-mode 'compilation-mode)
       (let ((inhibit-read-only t))
         (ansi-color-apply-on-region compilation-filter-start (point-max)))))))

(mmic eshell
  :custom
  ((eshell-directory-name
    . (expand-file-name
       "etc/eshell"
       user-emacs-directory)))
  :face
  ((eshell-prompt . ((t (:foreground "#00ff00"))))))

(mmic shell
  :hook
  ((shell-mode-hook . #'hook:shell-mode-coding))
  :eval
  ((defun hook:shell-mode-coding ()
     (pcase system-type
       (`windows-nt
        (set-process-coding-system (get-buffer-process (current-buffer)) 'utf-8-dos 'utf-8-dos))
       (_ (set-process-coding-system (get-buffer-process (current-buffer)) 'utf-8-unix 'utf-8-unix))))))

(mmic cmake-mode)

(mmic newsticker
  :custom
  ((newsticker-dir
    . (expand-file-name
       "etc/newsticker/"
       user-emacs-directory))))

(mmic ahk-mode)

(mmic editorconfig
  :eval
  ((editorconfig-mode)))

(mmic eldoc
  :custom
  ((eldoc-idle-delay . 1)
   (eldoc-echo-area-use-multiline-p . t)
   (eldoc-documentation-strategy . #'eldoc-documentation-compose-eagerly)))

;; (leaf flycheck
;;   :defun (global-flycheck-mode flycheck-error-message)
;;   :global-minor-mode global-flycheck-mode
;;   :custom
;;   (((flycheck-display-errors-function
;;      . 'flycheck-display-error-messages))
;;    (flycheck-idle-change-delay . 2)
;;    (flycheck-check-syntax-automatically
;;     . '(save idle-change new-line mode-enabled))
;;    (flycheck-disabled-checkers
;;     ;; too late
;;     . '(emacs-lisp-checkdoc
;;         ;; no include file
;;         c/c++-clang))))

(mmic flymake
  :hook
  ((prog-mode-hook . #'flymake-mode)))

(mmic flymake-elisp-config
  :eval
  ((flymake-elisp-config-global-mode)
   (flymake-elisp-config-auto-mode)))

(mmic flyspell
  :mykie
  ((global-map
    ("<f7>" :default flyspell-buffer :region flyspell-region))))

(mmic ispell
  :custom
  ((ispell-program-name . "hunspell"))
  :eval-after-load
  ((add-to-list
    'ispell-skip-region-alist
    '("\"\\([^\000-\377]\\|[ \n\t.,()0-9@:;/\\\\`{}*+<>?_]\\)+\""))))

(mmic apheleia
  :eval
  ((apheleia-global-mode)))

(mmic consult
  :define-key
  ((global-map
    ("C-s" . #'consult-line)
    ("C-x b" . #'consult-buffer)
    ("M-y" . #'consult-yank-from-kill-ring)
    ("M-g g" . #'consult-goto-line)
    ("M-g o" . #'consult-outline)
    ("M-g i" . #'consult-imenu)
    ("M-g I" . #'consult-imenu-multi)
    ("M-g m" . #'consult-mark)
    ("M-g M" . #'consult-global-mark)
    ("M-s f" . #'consult-find)
    ("M-s k" . #'consult-focus-lines)
    ("M-@" . #'consult-register-load)
    ("C-M-@" . #'consult-register-store)
    ("M-g h" . #'consult-org-heading)
    ("M-g a" . #'consult-org-agenda)
    ("M-g e" . #'consult-flymake)
    ("C-r" . #'consult-ripgrep)))
  :eval-after-load
  ((add-to-list 'consult-buffer-sources 'rhq-consult-source-project-directory 'append)))

;; (leaf consult-flycheck
;;   :bind
;;   (("M-g e" . consult-flycheck)))

(mmic embark
  :define-key
  ((global-map
    ("C-." . #'embark-act)
    ("M-." . #'embark-dwim)
    ("<help> B" . #'embark-bindings)))
  :define-key-after-load
  ((embark-file-map
    ("o" . (my/embark-ace-action find-file))
    ("2" . (my/embark-split-action find-file split-window-below))
    ("3" . (my/embark-split-action find-file split-window-right))
    ("S" . #'sudo-find-file))
   (embark-buffer-map
    ("o" . (my/embark-ace-action switch-to-buffer))
    ("2" . (my/embark-split-action switch-to-buffer split-window-below))
    ("3" . (my/embark-split-action switch-to-buffer split-window-right)))
   (embark-bookmark-map
    ("o" . (my/embark-ace-action bookmark-jump))
    ("2" . (my/embark-split-action bookmark-jump split-window-below))
    ("3" . (my/embark-split-action bookmark-jump split-window-right))))
  :eval
  ;; https://karthinks.com/software/fifteen-ways-to-use-embark/
  ((defmacro my/embark-ace-action (fn)
     `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
        (interactive)
        (with-demoted-errors "%s"
          (require 'ace-window)
          (let ((aw-dispatch-always t))
            (aw-switch-to-window (aw-select nil))
            (call-interactively (symbol-function ',fn))))))
   (defmacro my/embark-split-action (fn split-type)
     `(defun ,(intern (concat "my/embark-"
                              (symbol-name fn)
                              "-"
                              (car (last  (split-string
                                           (symbol-name split-type) "-"))))) ()
        (interactive)
        (funcall #',split-type)
        (call-interactively #',fn)))

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
                  (concat "/sudo:root@localhost:" file))))))

(mmic embark-consult
  :hook
  ((embark-collect-mode-hook . #'consult-preview-at-point-mode)))

(mmic marginalia
  :eval
  ((marginalia-mode)))

(mmic vertico
  :eval
  ((vertico-mode))
  :custom
  ((vertico-count . 30)))

(mmic orderless
  :custom
  ((completion-styles . '(orderless basic))
   (completion-category-overrides . '((file (styles orderless-migemo basic partial-completion))
                                      (consult-location (styles orderless-migemo basic partial-completion))
                                      (consult-org-heading (styles orderless-migemo basic partial-completion)))))
  :eval-after-load
  (
   ;; 1 character migemo regexp is too long
   (defun migemo-get-pattern-3 (word)
     (and (< 3 (length word))
          (migemo-get-pattern word)))

   (orderless-define-completion-style orderless-migemo
     (orderless-matching-styles '(orderless-literal
                                  orderless-regexp
                                  migemo-get-pattern-3)))))

(mmic corfu
  :eval
  ((global-corfu-mode)
   (corfu-popupinfo-mode))
  :custom
  ((corfu-auto . t))
  :eval
  ((add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))

(mmic kind-icon
  :custom
  ((kind-icon-default-face . 'corfu-default)))

(mmic cape
  :custom
  ((cape-dict-file
    . (cl-some
       (lambda (arg)
         (when (file-exists-p arg)
           (expand-file-name arg)))
       '("/etc/dictionaries-common/words"
         "~/.guix-profile/share/web2"))))
  :hook
  ((completion-at-point-functions . #'cape-file)
   (completion-at-point-functions . #'cape-keyword)
   (completion-at-point-functions . #'cape-dict)))

(mmic* history
  :custom
  ((history-length . t)))

(mmic* auto-save
  :custom
  ;; auto-save file
  ((delete-auto-save-files . t)
   (auto-save-file-name-transforms
    . `((".*" ,(expand-file-name "etc/backup/" user-emacs-directory) t)))
   (auto-save-list-file-prefix
    . (expand-file-name
       "etc/auto-save-list/.saves-"
       user-emacs-directory))
   (auto-save-timeout . 10)
   (auto-save-interval . 500)
   ;; backup file
   (make-backup-files . t)
   (backup-directory-alist
    . `((".*" . ,(expand-file-name
                  "etc/backup"
                  user-emacs-directory))))
   (version-control . t)
   (kept-new-versions . 5)
   (kept-old-versions . 1)
   (delete-old-versions . t)))

(mmic* savehist
  :custom
  ((savehist-file
    . (expand-file-name
       "etc/history"
       user-emacs-directory)))
  :eval
  ((savehist-mode)))

(mmic recentf
  :require t
  :custom
  ((recentf-max-saved-items . 200)
   (recentf-auto-cleanup . 'never)
   (recentf-save-file
    . (expand-file-name
       "etc/recentf"
       user-emacs-directory)))
  :eval
  ((recentf-mode)
   (add-to-list 'recentf-exclude ".*\\.emacs\\.d/elpa/.*")))

(mmic saveplace
  :custom
  ((save-place . t)
   (save-place-file
    . (expand-file-name "etc/.emacs-places" user-emacs-directory))))

(mmic rhq
  :require t
  :define-key
  ((global-map
    ("C-x C-p" . #'rhq-open-project-or-clone))))

(mmic transient
  :custom
  ((transient-history-file
    . (expand-file-name
       "etc/transient/history.el"
       user-emacs-directory))
   (transient-levels-file
    . (expand-file-name
       "etc/transient/levels.el"
       user-emacs-directory))
   (transient-values-file
    . (expand-file-name
       "etc/transient/values.el"
       user-emacs-directory))))

(mmic magit
  :define-key
  ((global-map
    ("M-g M-g" . #'magit-status))))

(mmic forge
  :custom
  ((forge-database-file
    . (expand-file-name "etc/forge-database.sqlite"
                        user-emacs-directory))))

(mmic series
  :require t
  :define-key-after-load
  ((global-map
    ("C-a" . #'series-home)
    ("C-e" . #'series-end)))
  :define-key-with-feature
  ((prog-mode
    (prog-mode-map
     ("C-a" . #'series-home-prog)
     ("C-e" . #'series-end-prog)))
   (eww
    (eww-mode-map
     ("a" . #'series-home-prog)
     ("e" . #'series-end-prog)))
   (org
    (org-mode-map
     ("C-a" . #'series-home-org)
     ("C-e" . #'series-end-org))))
  :eval
  ((defun forward-to-not-comment ()
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
       (concat "[ \t]*$") (buffer-substring (point-at-bol) (point-at-eol))))))
  :eval-after-load
  ((series-defun series-home
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
     series-return)))

(mmic bm
  :require t
  :custom
  ((bm-repository-file
    . (expand-file-name "etc/.bm-repository" user-emacs-directory))
   (bm-buffer-persistence . t))
  :define-key-general
  ((override
    ("C-M-n" . #'bm-next)
    ("C-M-p" . #'bm-previous)))
  :face
  ((bm-persistent-face
    . ((((class grayscale) (background light)) (:background "DimGray"))
       (((class grayscale) (background dark))  (:background "LightGray"))
       (((class color)     (background light)) (:foreground nil :background "#090960"))
       (((class color)     (background dark))  (:foreground nil :background "#090960")))))
  :hook
  ((after-init-hook . #'bm-repository-load)
   (find-file-hook  . #'bm-buffer-restore)
   (after-revert-hook . #'bm-buffer-restore)
   (kill-buffer-hook . #'bm-buffer-save)
   (after-save-hook . #'bm-buffer-save)
   (vc-before-checkin-hook . #'bm-buffer-save)
   (kill-emacs-hook . #'bm-buffer-save)
   (find-file-hook .  #'bm-buffer-restore)
   (kill-buffer-hook . #'bm-buffer-save)
   (after-save-hook . #'bm-buffer-save)
   (after-revert-hook . #'bm-buffer-restore)
   (vc-before-checkin-hook . #'bm-buffer-save)))

(mmic ace-jump-mode
  :define-key
  ((global-map
    ("C-:" . #'ace-jump-char-mode)))
  :custom
  ((ace-jump-mode-move-keys
    . (append "asdfghjkl;:qwertyuiopzxcvbnm,." nil)))
  :eval-after-load
  ((defun ace-jump-char-mode-migemo (query-char)
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
   (advice-add #'ace-jump-char-mode :override #'ace-jump-char-mode-migemo)))

(mmic ace-link
  :define-key-with-feature
  ((info
    (Info-mode-map
     (":" . #'ace-link-info)))
   (help-mode
    (help-mode-map
     (":" . #'ace-link-help)))
   (eww
    (eww-link-keymap
     (":" . #'ace-link-eww))
    (eww-mode-map
     ( ":" . #'ace-link-eww)))
   (org
    (org-mode-map
     ("C-c M-:" . #'ace-link-org)))))

(defun copy-all ()
  (interactive)
  (kill-ring-save (point-min) (point-max))
  (message "Copy all."))

(mmic* newcomment
  :mykie
  ((prog-mode-map
    ("M-;" :default comment-dwim :C-u comment-line)))
  :eval
  ((defun comment-line ()
     "Make the line comment."
     (interactive)
     (save-excursion
       (beginning-of-line)
       (set-mark (point))
       (end-of-line)
       (comment-or-uncomment-region (region-beginning) (region-end))))))


(mmic* key-translation
  :define-key
  ((key-translation-map
    ("C-h" . (kbd "DEL")))))

(mmic puni
  :define-key
  ((puni-mode-map
    ("C-S-h" . #'puni-splice)
    ("C-S-i" . #'puni-slurp-forward)
    ("C-S-o" . #'puni-barf-forward)
    ("M-I" . #'puni-slurp-backward)
    ("M-O" . #'puni-barf-backward)))

  :eval
  ((puni-global-mode)))

(mmic elec-pair
  :eval
  ((electric-pair-mode)))

(mmic smartparens
  :require t
  :define-key
  ((puni-mode-map
    ("M-s r" . #'sp-rewrap-sexp)
    ("C-M-d" . #'sp-delete-symbol)))
  :custom
  ((sp-highlight-pair-overlay . nil)
   (sp-base-key-bindings . 'sp)))

(mmic rainbow-delimiters
  :hook ((lisp-mode-hook . #'rainbow-delimiters-mode)
         (emacs-lisp-mode-hook . #'rainbow-delimiters-mode)
         (c-mode-common-hook . #'rainbow-delimiters-mode))
  :face
  ((rainbow-delimiters-unmatched-face
    . ((t (:foreground "#ff0000" :bold t :underline t))))
   (rainbow-delimiters-depth-1-face . ((t (:foreground "#bbffff"))))
   (rainbow-delimiters-depth-2-face . ((t (:foreground "#ffbbff"))))
   (rainbow-delimiters-depth-3-face . ((t (:foreground "#ffffaa"))))
   (rainbow-delimiters-depth-4-face . ((t (:foreground "#aaddaa"))))
   (rainbow-delimiters-depth-5-face . ((t (:foreground "#ff55ff"))))
   (rainbow-delimiters-depth-6-face . ((t (:foreground "#09d999"))))
   (rainbow-delimiters-depth-7-face . ((t (:foreground "#ff6666"))))
   (rainbow-delimiters-depth-8-face . ((t (:foreground "#66ff66"))))
   (rainbow-delimiters-depth-9-face . ((t (:foreground "#6666ff"))))))

(mmic smart-newline
  :hook
  ((prog-mode-hook . #'smart-newline-mode)
   (text-mode-hook . #'smart-newline-mode)
   (makefile-mode-hook . #'smart-newline-mode-off)
   (makefile-gmake-mode-hook . #'smart-newline-mode-off)
   (yaml-mode-hook . #'smart-newline-mode-off))
  :eval
  ((defun smart-newline-mode-off ()
     (smart-newline-mode -1))))

(mmic electric-operator
  :hook ((c-mode-common-hook . #'electric-operator-mode)
         (typescript-mode-hook . #'electric-operator-mode)
         (ess-r-mode-hook . #'electric-operator-mode)))

(mmic expand-region
  :define-key-with-feature
  ((region-bindings-mode
    (region-bindings-mode-map
     ("C-SPC" . #'er/expand-region)))))

(mmic visual-regexp
  :define-key
  ((global-map
    ("C-c r" . #'vr/query-replace))))

(mmic grugru
  :custom
  ((grugru-edit-save-file . (expand-file-name ".grugru" user-emacs-directory)))
  :define-key
  ((global-map
    ("C-;" . #'grugru)
    ("<C-tab>" . #'grugru)))
  :require t
  :eval
  ((grugru-highlight-mode)
   (grugru-default-setup)
   (grugru-edit-load)))

(mmic* undo
  :custom
  ((undo-outer-limit . nil)))

(mmic winner
  :eval
  ((winner-mode)))

(mmic undo-tree
  :custom
  ((undo-tree-auto-save-history . nil))
  :hydra
  ((hydra-undo
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
    ("<right>" winner-redo)))
  :define-key
  ((global-map
    ("C-x u" . #'hydra-undo/undo-tree-undo)
    ("M-u" . #'hydra-undo/undo-tree-undo)))
  :define-key-after-load
  ((undo-tree-map
    ("C-x u" . #'hydra-undo/undo-tree-undo)
    ("C-/" . #'hydra-undo/undo-tree-undo)))
  :eval
  ((global-undo-tree-mode)))

(mmic* mule-cmds
  :define-key
  ((global-map
    ("<help> h" . nil))))

(mmic* faces
  :face
  ((help-argument-name . ((t (:bold t :italic t))))))

(mmic find-func
  :custom
  ((find-function-C-source-directory
    . (expand-file-name
       "../src" (invocation-directory)))))

(mmic helpful
  :define-key
  ((global-map
    ("<help> k" . #'helpful-key)
    ("<help> f" . #'helpful-callable)
    ("<help> v" . #'helpful-variable))))

(mmic which-key
  :custom
  ((which-key-idle-delay . 1))
  :eval
  ((which-key-mode)
   (which-key-setup-side-window-bottom)))

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

(mmic info
  :eval
  ((defvar my-Info-substitute
     '(("emacs" . "emacs-ja")
       ("org" . "org-ja")
       ("magit" . "magit.ja")
       ("magit-section" . "magit-section.ja")))
   (defun Info-find-node--info-ja (args)
     (let ((filename (car args))
           (rest (cdr args)))
       (cons
        (alist-get filename my-Info-substitute filename nil #'string=)
        rest)))
   (advice-add #'Info-find-node :filter-args #'Info-find-node--info-ja)
   (add-to-list 'Info-directory-list
                (expand-file-name "info/" user-emacs-directory))))

(defcustom my-deepl-api-key nil
  "My DeepL API key.")

;; (leaf* mine
;;   :ensure nil
;;   :pl-custom
;;   (my-deepl-api-key))

(mmic go-translate
  :custom
  ((gts-translate-list . '(("en" "ja") ("ja" "en"))))
  :custom-after-load
  ((my-deepl-api-key . nil)
   (gts-default-translator
    . (gts-translator
       :picker (gts-prompt-picker :texter (gts-current-or-selection-texter))
       :engines
       (list
        (gts-deepl-engine :auth-key my-deepl-api-key :pro nil))
       :render
       (gts-buffer-render)))))

(set-language-environment "Japanese")

(mmic* kkc
  :custom
  ((kkc-init-file-name
    . (expand-file-name
       "etc/kkcrc" user-emacs-directory))))

(mmic* skk
  :package ddskk
  :define-key-after-load
  ((skk-j-mode-map
    ("\\" . #'self-insert-command)
    ("$"  . #'self-insert-command)))
  :hook
  ((skk-mode-hook . #'skk-yatex-hook))
  :eval
  ((defun skk-yatex-hook ()
     (when (eq major-mode 'YaTeX-mode)
       (make-local-variable 'skk-j-mode-map)
       (define-key skk-j-mode-map "$" 'YaTeX-insert-dollar))))
  :custom
  ((skk-inhibit-ja-dic-search . t)
   (skk-jisyo
    . (expand-file-name "etc/.skk-jisyo" user-emacs-directory))
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
   (skk-sticky-key . (kbd
                      (pcase system-type
                        (`windows-nt "<non-convert>")
                        (_ "<muhenkan>"))))
   (skk-search-katakana . t)
   (skk-japanese-message-and-error . t))
  :face
  ((skk-dcomp-multiple-face
    . ((t (:foreground "black" :background "gray" :bold nil))))
   (skk-dcomp-multiple-trailing-face
    . ((t (:foreground "white" :bold nil))))
   (skk-dcomp-multiple-selected-face
    . ((t (:foreground "white" :background "steel blue" :bold nil))))

   (skk-dcomp-face . '((t (:foreground "#dfdfdf")))))
  :eval
  ((add-to-list 'skk-rom-kana-rule-list '("z:" nil (":" . ":")) t)))

(mmic jaword
  :eval
  ((global-jaword-mode)))

(mmic migemo
  :require t
  :custom
  ((migemo-command . "cmigemo")
   (migemo-options . '("-q" "--emacs"))
   ;; Set your installed path
   (migemo-dictionary
    . (pcase system-type
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
  :eval-after-load
  ((migemo-init)))

(mmic open-junk-file
  :define-key
  ((global-map
    ("C-x s" . #'open-junk-file)))
  :custom
  ((open-junk-file-format
    . (expand-file-name
       "junk/%Y-%m-%d-%H%M%S."
       (getenv "HOME")))))

(mmic oj
  :custom
  ((oj-home-dir . (expand-file-name "oj" "~"))
   (oj-default-online-judge . 'atcoder))
  :define-key
  ((global-map
    ("C-x j" . (make-sparse-keymap))
    ("C-x j t" . #'oj-test)
    ("C-x j s" . #'oj-submit)
    ("C-x j n" . #'oj-next)
    ("C-x j p" . #'oj-prev)
    ("C-x j P" . #'oj-prepare))))

(mmic ssh
  :custom
  ((ssh-program . "plink")
   (tramp-default-method . "psftp"))
  :hook
  ((ssh-mode-hook . #'ssh-directory-tracking-mode)
   (ssh-mode-hook . #'shell-dirtrack-mode)))

(mmic yasnippet
  :package (yasnippet-snippets)
  :require (t yasnippet-snippets)
  :pretty-hydra
  (( hydra-yas (:color blue)
     ("YASnippet"
      (("n" yas-new-snippet "New")
       ("e" yas-visit-snippet-file "Edit")
       ("i" yas-insert-snippet "Insert")
       ("q" nil)))))
  :define-key
  ((yas-minor-mode-map
    ("<deletechar>" . #'yas-skip-and-clear-or-delete-char)
    ("<tab>" . nil)
    ("TAB" . nil)
    ("C-x y" . #'hydra-yas/body)))
  :define-key-after-load
  ((yas-minor-mode-map
    ("SPC" . yas-maybe-expand)))
   :custom
  ;; expand yasnippet only beginning of symbol.
  ((yas-key-syntaxes . '("w_"))
   (yas-also-auto-indent-first-line . t))
  :custom-after-load
  ((yas-buffer-local-condition . yas-not-string-or-comment-condition)
   (yas-snippet-dirs
    . (cons 'yasnippet-snippets-dir
            (my-standard-value 'yas-snippet-dirs))))
  :hook
  ((snippet-mode-hook . #'hook:snippet/kill-whitespace-trancation))
  :eval
  ((add-to-list 'auto-mode-alist '("snippets/" . snippet-mode))
   (yas-global-mode)
   (defun hook:snippet/kill-whitespace-trancation ()
     (remove-hook 'before-save-hook #'delete-trailing-whitespace t))))

(mmic* char-code
  :custom
  ((buffer-file-coding-system . 'utf-8))
  :eval
  ((defalias 'mojibake 'revert-buffer-with-coding-system)
   (defalias 'mojibakejanai 'set-buffer-file-coding-system)
   (when (eq system-type 'gnu/linux)
     (setq default-file-name-coding-system 'utf-8))))

(mmic* doc
  :custom
  ((text-quoting-style . 'straight)))

(mmic* visual
  :custom
  ((use-dialog-box . nil)
   (echo-keystrokes . 0.01)))

(mmic highlight-indent-guides
  :hook
  ((yaml-mode-hook . #'highlight-indent-guides-mode))
  :custom
  ((highlight-indent-guides-method . 'fill)
   (highlight-indent-guides-auto-enabled . t)
   (highlight-indent-guides-responsive . t)
   (highlight-indent-guides-delay . 0.9)))

(mmic highlight-defined
  :hook
  ((emacs-lisp-mode-hook . #'highlight-defined-mode)))

(mmic volatile-highlights
  :eval
  ((volatile-highlights-mode)))

(mmic whitespace
  :face
  ((whitespace-space
    . ((t (:foreground "#ffb90f" :background nil :underline t :bold t))))
   (whitespace-tab
    . ((t (:foreground "#caff70" :background nil :underline t)))))
  :custom
  ((whitespace-style
    . '(face tabs tab-mark spaces space-mark newline newline-mark))
   (whitespace-space-regexp . "\\(\x3000+\\)")
   (whitespace-display-mappings
    . '((tab-mark   ?\t   [?\xBB ?\t]))))
  :eval
  ((global-whitespace-mode)

   (defun trailing-whitespace-off ()
     (interactive)
     (setq show-trailing-whitespace nil))))

(mmic rainbow-mode
  :hook
  ((emacs-lisp-mode-hook . #'rainbow-mode)))

(mmic color-identifiers-mode
  :hook ((c-mode-common-hook . #'color-identifiers-mode))
  :custom
  ((color-identifiers:num-colors . 20)))

(defalias 'messasge-box 'message)

(when (display-graphic-p)
  (mmic* font
    :custom
    ((use-default-font-for-symbols . nil)))
  (mmic font-lock-studio
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
  (set-face-attribute 'fixed-pitch nil :family 'unspecified))

(mmic* my-modeline
  :require t)

(add-to-list 'default-frame-alist '(cursor-type . box))

;; theme load
(mmic* theme
  :custom
  ((custom-theme-directory . (expand-file-name "themes/" user-emacs-directory)))
  :eval-after-others
  ;; (load-theme 'my-dark-green t)
  ((load-theme 'my-dark-cream t)))

(mmic view
  :define-key
  ((global-map
    ("<f9>" . view-mode)))
  :eval
  ((defvar view-mode-off-hook nil)
   (defun my:view-mode-off-hook (&rest _)
     (run-hooks 'view-mode-off-hook))
   (advice-add #'view--disable :after #'my:view-mode-off-hook)
   (defun my:view-color ()
     (if view-mode
         (face-remap-add-relative 'mode-line
                                  :background my:color-view-mode-enabled)
       (face-remap-add-relative 'mode-line
                                :background my:color-modeline-background))))
  :hook
  ((view-mode-hook .  #'my:view-color)
   (view-mode-off-hook . #'my:view-color)))

(mmic hideshow
  :hook
  ((prog-mode-hook . #'hs-minor-mode))
  :eval
  ((defun my-hs-hide-level ()
     (interactive)
     (hs-hide-level 0)))
  :define-key-after-load
  ((hs-minor-mode-map
    ("C-\\"  . #'hs-toggle-hiding)
    ("M-s s" . #'hs-show-all)))
  :eval-after-load
  ((mykie:define-key hs-minor-mode-map "M-s h"
     :default my-hs-hide-level :C-u hs-hide-all)))

(mapc
 (lambda (arg)
   (put arg 'disabled nil))
 '(downcase-region
   narrow-to-region
   list-timers
   upcase-region))

(when my-profiler-on
  (add-hook 'after-init-hook #'profiler-report 100)
  (add-hook 'after-init-hook #'profiler-stop 110))

(provide 'init)

;;; init.el ends here
