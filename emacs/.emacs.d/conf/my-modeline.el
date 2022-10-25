(require 'cl-lib)

;;; Define colors

(defconst my:color-major-mode                   "#5d478b")
(defconst my:color-buffer-name                  "#6495ed")
(defconst my:color-elscreen-number              "#88bb23")
(defconst my:color-branch                       "#d3d3d3")
(defconst my:color-region                       "#eedd82")
(defconst my:color-battery-danger               "#ff0000")
(defconst my:color-battery-middle               "#dddd00")
(defconst my:color-battery-safe                 "#32cd32")
(defconst my:color-view-mode-enabled            "#2e8b57")
(defconst my:color-modeline-background          "maroon4")
(defconst my:color-modeline-foreground          "#fffacd")
(defconst my:color-modeline-inactive-background "thistle4")
(defconst my:color-modeline-flycheck-background "#000000")
(defconst my:color-modeline-flycheck-foreground "#00ff00")
(defconst my:color-modeline-black               "black")


;;; Define faces

;; n: None
;; m: major-mode
;; b: buffer-name
;; r: region
;; p: project branch
;; d: battery
;; e: elscreen-number
;; B: black
;; mode-line-color-<foreground><background>
(make-face 'mode-line-color-nm)
(set-face-attribute
 'mode-line-color-nm nil
 :foreground my:color-modeline-foreground :bold t
 :background my:color-major-mode)

(make-face 'mode-line-color-nb)
(set-face-attribute 'mode-line-color-nb nil
                    :foreground my:color-modeline-foreground :bold t
                    :background my:color-buffer-name)
(make-face 'mode-line-color-ne)
(set-face-attribute 'mode-line-color-ne nil
                    :foreground my:color-modeline-foreground :bold t
                    :background my:color-elscreen-number)
(make-face 'mode-line-color-Bp)
(set-face-attribute 'mode-line-color-Bp nil
                    :foreground my:color-modeline-black :bold t
                    :background my:color-branch)

(make-face 'mode-line-error)
(set-face-attribute 'mode-line-error nil
                    :inherit 'error
                    :background "black")
(make-face 'mode-line-warning)
(set-face-attribute 'mode-line-warning nil
                    :inherit 'warning
                    :background "black")

(make-face 'mode-line-color-region)
(set-face-attribute 'mode-line-color-region nil
                    :inherit 'default :bold t
                    :background my:color-region)

(make-face 'mode-line-color-battery-danger)
(set-face-attribute 'mode-line-color-battery-danger nil
                    :foreground my:color-modeline-foreground :bold t
                    :background my:color-battery-danger)
(make-face 'mode-line-color-battery-middle)
(set-face-attribute 'mode-line-color-battery-middle nil
                    :foreground my:color-modeline-black :bold t
                    :background my:color-battery-middle)
(make-face 'mode-line-color-battery-safe)
(set-face-attribute 'mode-line-color-battery-safe nil
                    :foreground my:color-modeline-black :bold t
                    :background my:color-battery-safe)
(make-face 'flycheck-modeline-color)
(set-face-attribute 'flycheck-modeline-color nil
                    :foreground my:color-modeline-flycheck-foreground
                    :background my:color-modeline-flycheck-background)
(make-face 'flycheck-modeline-color-note)
(set-face-attribute 'flycheck-modeline-color-note nil
                    :foreground my:color-modeline-flycheck-foreground
                    :background my:color-modeline-flycheck-background
                    :bold t)
(display-time)
(display-battery-mode)

(make-face 'my:region-face)
(set-face-attribute 'my:region-face nil
                    :inherit 'default :bold t)

(make-face 'my:branch-icons)
(set-face-attribute 'my:branch-icons nil
                    :background my:color-branch
                    :foreground my:color-modeline-black)


;;; Major mode icon

;; Display icon instaed of major-mode
(defvar my:major-mode-string-alist
  '((c-mode   . "\xe61e")
    (c++-mode . "\xe61d")
    (html-mode . "\xe60e")
    (web-mode . "\xe60e")
    (python-mode . "\xe235")
    ;; (dired-mode . "\xe5fe")
    (markdown-mode . "\xe614")
    (php-mode . "\xe73d")
    (magit-status-mode . "\xe702")
    (shell-script-mode . "\xf085")
    (shell-mode . "\xf489")
    (text-mode . "\xf4a5")
    (java-mode . "\xe256")
    (dockerfile-mode . "\xe7b0")
    (org-mode . "\xf069")
    (clojure-mode . "\xe768")
    (javascript-mode . "\xe60c")
    (typescript-mode . "\xe628")
    (diff-mode . "\xf440")))

(defun my:major-mode-string ()
  (let ((str (assq major-mode my:major-mode-string-alist)))
    (if (and str (font-info "Cica"))
        (cdr str) (format-mode-line mode-name))))

(set-face-attribute 'my:region-face nil
                    :inherit 'region)


;;; Apply mode-line

(setq-default
 mode-line-format
 (list
  '(:eval
    (when mark-active
      (progn
        (concat
         (propertize
          (format " %dLine%dWord%dChar"
                  (count-lines (region-beginning) (region-end))
                  (how-many "[^\t\n ]+" (region-beginning) (region-end))
                  (- (region-end) (region-beginning)))
          'face 'my:region-face)
         (propertize " " 'face 'my:region-face)))))

  ;; narrow or not
  '(:eval
    (if (buffer-narrowed-p)
        (if (font-info "Cica") "\xf066" "n")
      " "))
  ;; other
  'mode-line-mule-info
  'mode-line-modified

  " "
  ;; major-mode
  '(:eval (concat (propertize
                   (concat " " (my:major-mode-string) " ")
                   'face 'mode-line-color-nm)))
  " "
  ;; buffer-name
  '(:eval (concat (propertize " %b " 'face 'mode-line-color-nb)))

  "  "
  mode-line-position

  ;; flycheck
  '(:eval
    (cond
     ((and
       (featurep 'flycheck)
       (not
        (memq flycheck-last-status-change
              '(not-checked no-checker))))
      (concat
       " "
       (propertize " " 'face 'flycheck-modeline-color)

       (if (eq flycheck-last-status-change 'finished)
           (let-alist (flycheck-count-errors flycheck-current-errors)
             (if (or .error .warning)
                 (concat
                  (propertize (number-to-string (or .error 0))
                              'face 'mode-line-error)
                  (propertize "/" 'face 'flycheck-modeline-color)
                  (propertize (number-to-string (or .warning 0))
                              'face 'mode-line-warning))
               (propertize "-" 'face 'flycheck-modeline-color)))
         (propertize
          (pcase flycheck-last-status-change
            (`running "*")
            (`errored "!")
            (`interrupted ".")
            (`suspicious "?"))
          'face 'flycheck-modeline-color))

       (propertize " " 'face 'flycheck-modeline-color)
       " "))
     (t
      (concat
       " "
       (propertize " " 'face 'flycheck-modeline-color)
       (let ((error (substring-no-properties (cadadr (flymake--mode-line-counter :error t))))
             (warning (substring-no-properties (cadadr (flymake--mode-line-counter :warning t))))
             (note (substring-no-properties (or (cadadr (flymake--mode-line-counter :note t)) "0"))))
         (if (not (cl-every (apply-partially #'string= "0") (list error warning note)))
           (concat
            (propertize error
                        'face 'mode-line-error)
            (propertize "/" 'face 'flycheck-modeline-color)
            (propertize warning
                        'face 'mode-line-warning)
            (propertize "/" 'face 'flycheck-modeline-color)
            (propertize note
                        'face 'flycheck-modeline-color-note))
         (propertize "-" 'face 'flycheck-modeline-color)))
       (propertize " " 'face 'flycheck-modeline-color)
       " "))))


  mode-line-process
  " "

  ;; LSP-mode
  '(:eval (when (and (featurep 'lsp-mode)
                     (featurep 'lsp-modeline)
                     lsp--buffer-workspaces)
            (format-mode-line
             '("["
               (:eval (mapconcat #'lsp--workspace-print lsp--buffer-workspaces "]["))
               "] "
               lsp-modeline--code-actions-string
               " "
               (:eval (format-mode-line (lsp-modeline--diagnostics-update-modeline)))
               " "
               (:eval (format-mode-line (lsp-modeline--workspace-status-string)))))))

  '(:eval (when (and (featurep 'eglot)
                     eglot--managed-mode)
            (concat (format-mode-line (eglot--mode-line-format))
                    "/"
                    (plist-get (eglot--server-info (eglot-current-server)) :name)
                    )))))


;;; Set faces

(set-face-attribute
 'mode-line nil
 :foreground my:color-modeline-foreground
 :background my:color-modeline-background
 :box nil)
(set-face-attribute
 'mode-line-inactive nil
 :foreground my:color-modeline-foreground
 :background my:color-modeline-inactive-background
 :box nil)

(make-face 'mode-line-view-mode-enabled)
(set-face-attribute
 'mode-line-view-mode-enabled nil
 :foreground my:color-modeline-foreground
 :background my:color-view-mode-enabled)

(provide 'my-modeline)
