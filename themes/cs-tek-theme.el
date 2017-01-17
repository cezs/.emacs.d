(deftheme cs-tek
  "Created 2016-10-02.")

(custom-theme-set-faces
 'cs-tek
 '(default ((t (:inherit nil :stipple nil :background "#015f51" :foreground "#35f9c9" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "PfEd" :family "DejaVu Sans Mono"))))
 '(cursor ((t (:background "#35f9c9" :foreground "#015f51"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((t (:family "Sans Serif"))))
 '(escape-glyph ((t (:foreground "#35f9c9"))))
 '(minibuffer-prompt ((t (:foreground "#35f9c9" :weight bold))))
 '(highlight ((t (:background "#35f9c9" :foreground "#015f51"))))
 '(region ((t (:background "#35f9c9" :foreground "#015f51"))))
 '(shadow ((t (:foreground "#35f9c9"))))
 '(secondary-selection ((t (:background "#35f9c9" :foreground "#015f51"))))
 '(trailing-whitespace ((t (:background "#35f9c9" :foreground "#015f51"))))
 '(font-lock-builtin-face ((t (:foreground "#35f9c9"))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face :foreground "#35f9c9"))))
 '(font-lock-comment-face ((t (:foreground "#35f9c9"))))
 '(font-lock-constant-face ((t (:foreground "#35f9c9"))))
 '(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
 '(font-lock-function-name-face ((t (:foreground "#35f9c9" :weight bold))))
 '(font-lock-keyword-face ((t (:foreground "#35f9c9" :weight bold))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "#35f9c9"))))
 '(font-lock-type-face ((t (:foreground "#35f9c9"))))
 '(font-lock-variable-name-face ((t (:foreground "#35f9c9"))))
 '(font-lock-warning-face ((t (:foreground "#35f9c9" :weight bold))))
 '(button ((t (:underline (:color foreground-color :style line)))))
 '(link ((t (:foreground "#35f9c9" :underline t))))
 '(link-visited ((t (:foreground "#35f9c9" :underline t))))
 '(fringe ((t (:background "#35f9c9" :foreground "#015f51"))))
 '(header-line ((t (:background "#35f9c9" :foreground "#015f51"))))
 '(tooltip ((t (:inherit variable-pitch :background "#35f9c9" :foreground "#015f51"))))
 '(mode-line ((t (:background "#35f9c9" :foreground "#015f51"))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "grey40" :style released-button))) (t (:inherit (highlight)))))
 '(mode-line-inactive ((t (:background "#015f51" :foreground "#35f9c9"))))
 '(isearch ((t (:background "#35f9c9" :foreground "#015f51"))))
 '(isearch-fail ((t (:background "#35f9c9" :foreground "#015f51"))))
 '(lazy-highlight ((t (:background "#35f9c9" :foreground "#015f51"))))
 '(match ((t (:background "#35f9c9" :foreground "#015f51"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit isearch :background "#35f9c9" :foreground "#015f51"))))
 '(org-done ((t (:foreground "#35f9c9" :weight bold))))
 '(org-todo ((t (:foreground "#35f9c9" :weight bold))))
 '(org-date ((t (:foreground "#35f9c9" :underline t)))))

(provide-theme 'cs-tek)
