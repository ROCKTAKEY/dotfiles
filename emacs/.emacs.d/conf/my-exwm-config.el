(when (require 'exwm nil t)
  (require 'exwm-config)
  (unless (get 'exwm-workspace-number 'saved-value)
    (setq exwm-workspace-number 4))
  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))
  ;; Global keybindings.
  (unless (get 'exwm-input-global-keys 'saved-value)
    (setq exwm-input-global-keys
          `(
            ;; 's-r': Reset (to line-mode).
            ([?\s-r] . exwm-reset)
            ;; 's-w': Switch workspace.
            ([?\s-w] . exwm-workspace-switch)
            ;; 's-&': Launch application.
            ([?\s-&] . (lambda (command)
                         (interactive (list (read-shell-command "$ ")))
                         (start-process-shell-command command nil command)))
            ;; 's-N': Switch to certain workspace.
            ,@(mapcar (lambda (i)
                        `(,(kbd (format "s-%d" i)) .
                          (lambda ()
                            (interactive)
                            (exwm-workspace-switch-create ,i))))
                      (number-sequence 0 9)))))
  ;; Line-editing shortcuts
  (unless (get 'exwm-input-simulation-keys 'saved-value)
    (setq exwm-input-simulation-keys
          '(([?\C-b] . [left])
            ([?\C-f] . [right])
            ([?\C-p] . [up])
            ([?\C-n] . [down])
            ([?\C-a] . [home])
            ([?\C-e] . [end])
            ([?\M-v] . [prior])
            ([?\C-v] . [next])
            ([?\C-d] . [delete])
            ([?\C-h] . [backspace])
            ([?\C-k] . [S-end delete]))))

  (mapc
   (lambda (cons)
     (define-key exwm-mode-map (kbd (car cons)) (cdr cons)))
   '(("C-q" . exwm-input-send-next-key)
     ("M-q" . major-mode-hydra)
     ("M-o" . ace-window)
     ("C-o" . other-window)))

  ;; Enable EXWM
  (exwm-enable)

  (prog1 (require 'exwm-randr)
    (setq exwm-randr-workspace-output-plist '( 0 "HDMI-2"
                                               1 "DP-2"))
    (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (call-process
               (expand-file-name "~/.screenlayout/main.sh"))))
    ;; (defun exwm-change-screen-hook ()
    ;;   (let ((xrandr-output-regexp "\n\\([^ ]+\\) connected ")
    ;;         default-output)
    ;;     (with-temp-buffer
    ;;       (call-process "xrandr" nil t nil)
    ;;       (goto-char (point-min))
    ;;       (re-search-forward xrandr-output-regexp nil 'noerror)
    ;;       (setq default-output (match-string 1))
    ;;       (forward-line)
    ;;       (if (not (re-search-forward xrandr-output-regexp nil 'noerror))
    ;;           (call-process "xrandr" nil nil nil "--output" default-output "--auto")
    ;;         (call-process
    ;;          "xrandr" nil nil nil
    ;;          "--output" (match-string 1) "--primary" "--auto"
    ;;          "--output" default-output "--off")
    ;;         (setq exwm-randr-workspace-output-plist (list 0 (match-string 1)))))))
    ;; (add-hook 'exwm-randr-screen-change-hook #'exwm-change-screen-hook)

    (exwm-randr-enable))

  (prog1 (require 'exwm-systemtray)
    (exwm-systemtray-enable)))

(provide 'my-exwm-config)
