(deftheme my-dark-gray
  "my dark-gray theme.")

(custom-theme-set-faces
 'my-dark-gray
 '(cursor  ((t (:foreground "#222222" :background "gray"))))
 '(default ((t (:background "#222222" :foreground "gray"))))
 '(region  ((t (:background "blue4"))))
 '(fringe  ((t (:background "#282828"))))
 '(error   ((t (:foreground "red" :bold t))))
 '(line-number ((t (:background "#282828" :foreground "#aaaaaa"))))
 '(font-lock-builtin-face       ((t (:foreground "#88bb88"))))
 '(font-lock-string-face        ((t (:foreground "#e55555"))))
 '(font-lock-comment-face       ((t (:foreground "gray45"))))
 '(font-lock-keyword-face       ((t (:foreground "#d343f3"))))
 '(font-lock-function-name-face ((t (:foreground "#7777ff"))))
 '(font-lock-constant-face      ((t (:foreground "#22adad"))))
 '(font-lock-variable-name-face ((t (:foreground "#edaa00"))))
 '(font-lock-type-face          ((t (:foreground "light cyan"))))
 ;; highlight-indent-guides
 '(highlight-indent-guides-odd-face       ((t (:background "#471015"))))
 '(highlight-indent-guides-even-face      ((t (:background "#13005b"))))
 '(highlight-indent-guides-character-face ((t (:foreground "dark gray"))))
 ;; highlight-defined
 '(highlight-defined-function-name-face   ((t (:foreground "#7777ff"))))
 '(highlight-defined-variable-name-face   ((t :foreground "#edaa00")))
 '(highlight-defined-face-name-face       ((t (:foreground "LightGoldenrod"))))
 )
