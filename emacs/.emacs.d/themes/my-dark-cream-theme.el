(deftheme my-dark-cream
  "my dark-cream theme.")

(custom-theme-set-faces
 'my-dark-cream
 '(cursor      ((t (:foreground "#272727" :background "#faf5d0"))))
 '(default     ((t (:background "#272727" :foreground "#faf5d0"))))
 '(fringe      ((t (:background "#282828"))))
 '(region      ((t (:background "#000066"))))
 '(line-number ((t (:background "#282828" :foreground "#aaaaaa"))))
 '(header-line ((t (:background nil :foreground nil :inherit default))))
 '(error       ((t (:foreground "#ff1111" :bold t))))
 '(show-paren-match ((t (:background "DodgerBlue4"))))
 ;; font-lock
 '(font-lock-warning-face       ((t (:foreground "#ff5522" :bold t))))
 '(font-lock-builtin-face       ((t (:foreground "#cceecc"))))
 '(font-lock-string-face        ((t (:foreground "#f5a5a5"))))
 '(font-lock-comment-face       ((t (:foreground "gray45"))))
 '(font-lock-keyword-face       ((t (:foreground "#ef6fff"))))
 '(font-lock-function-name-face ((t (:foreground "#aaaaff"))))
 '(font-lock-constant-face      ((t (:foreground "#42fced"))))
 '(font-lock-variable-name-face ((t (:foreground "#fdca66"))))
 '(font-lock-type-face          ((t (:foreground "#c0ffdf"))))
 ;; highlight-indent-guides
 '(highlight-indent-guides-odd-face       ((t (:background "#572035"))))
 '(highlight-indent-guides-even-face      ((t (:background "#33207b"))))
 '(highlight-indent-guides-character-face ((t (:foreground "dark gray"))))
 ;; highlight-defined
 '(highlight-defined-function-name-face   ((t (:foreground "#aaaaff"))))
 '(highlight-defined-variable-name-face   ((t :foreground "#fdca66")))
 '(highlight-defined-face-name-face       ((t (:foreground "LightGoldenrod"))))
 ;; ido
 '(ido-only-match  ((t (:foreground "#88ff66" :bold t))))
 '(ido-subdir      ((t (:foreground "#ffaaaa"))))
 '(ido-first-match ((t (:foreground "#97defa" :bold t :underline t))))
 ;; ido-flex-with-migemo
 '(ido-flex-with-migemo-migemo-face ((t (:background "#0e602f" :italic t))))
 ;; ido-grid
 '(ido-grid-mode-jump-face ((t (:foreground "red" :background "#000000"))))
 ;; volatile-highlight
 '(vhl/default-face ((t (:background "#0e2b00"))))
 ;; hydra
 '(hydra-face-red ((t (:foreground "#ff6666" :bold t))))
 ;; tab-bar
 '(tab-bar ((t (:background "#101010"))))
 '(tab-bar-tab ((t (:background "#272727" :foreground "#bbbbbb" :box (:line-width 6 :color "#272727") :italic t))))
 '(tab-bar-tab-inactive ((t (:background "#101010" :foreground "#999999" :box (:line-width 6 :color "#101010") :italic t)))))
