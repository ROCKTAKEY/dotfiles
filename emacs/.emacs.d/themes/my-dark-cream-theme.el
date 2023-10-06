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
 ;; `ansi-color'
 '(ansi-color-blue  ((t (:foreground "#aaaaff" :background "#aaaaff"))))
 '(ansi-color-red ((t (:foreground "#ff5522" :background "#ff5522"))))
 '(ansi-color-magenta ((t (:foreground "#ef6fff" :background "#ef6fff"))))
 '(ansi-color-green ((t (:foreground "#aaffaa" :background "#aaffaa"))))
 ;; `ace-window'
 '(aw-leading-char-face ((t (:foreground "red"))))
 '(aw-mode-line-face ((t (:background "#006000" :foreground "white"))))
 ;; `font-lock'
 '(font-lock-warning-face       ((t (:foreground "#ff5522" :bold t))))
 '(font-lock-builtin-face       ((t (:foreground "#cceecc"))))
 '(font-lock-string-face        ((t (:foreground "#f5a5a5"))))
 '(font-lock-comment-face       ((t (:foreground "gray45"))))
 '(font-lock-keyword-face       ((t (:foreground "#ef6fff"))))
 '(font-lock-function-name-face ((t (:foreground "#aaaaff"))))
 '(font-lock-constant-face      ((t (:foreground "#42fced"))))
 '(font-lock-variable-name-face ((t (:foreground "#fdca66"))))
 '(font-lock-type-face          ((t (:foreground "#c0ffdf"))))
 ;; `highlight-indent-guides'
 '(highlight-indent-guides-odd-face       ((t (:background "#572035"))))
 '(highlight-indent-guides-even-face      ((t (:background "#33207b"))))
 '(highlight-indent-guides-character-face ((t (:foreground "dark gray"))))
 ;; `highlight-defined'
 '(highlight-defined-function-name-face   ((t (:foreground "#aaaaff"))))
 '(highlight-defined-variable-name-face   ((t :foreground "#fdca66")))
 '(highlight-defined-face-name-face       ((t (:foreground "LightGoldenrod"))))
 ;; `volatile-highlight'
 '(vhl/default-face ((t (:background "#0e2b00"))))
 ;; `bm'
 '(bm-persistent-face
   ((((class grayscale) (background light)) (:background "DimGray"))
    (((class grayscale) (background dark))  (:background "LightGray"))
    (((class color)     (background light)) (:foreground nil :background "#090960"))
    (((class color)     (background dark))  (:foreground nil :background "#090960"))))
 ;; `hydra'
 '(hydra-face-red ((t (:foreground "#ff6666" :bold t))))
 ;; `tab-bar'
 '(tab-bar ((t (:background "#101010"))))
 '(tab-bar-tab ((t (:background "#272727" :foreground "#bbbbbb" :box (:line-width 6 :color "#272727") :italic t))))
 '(tab-bar-tab-inactive ((t (:background "#101010" :foreground "#999999" :box (:line-width 6 :color "#101010") :italic t))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "#ff0000" :bold t :underline t))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#bbffff"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#ffbbff"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#ffffaa"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#aaddaa"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#ff55ff"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#09d999"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#ff6666"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#66ff66"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#6666ff"))))
 '(whitespace-space
   ((t (:foreground "black" :background "#ffb90f"))))
 '(whitespace-tab
   ((t (:foreground "black" :background "#caff70")))))
