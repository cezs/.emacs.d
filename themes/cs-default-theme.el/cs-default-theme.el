;;; cs-default-theme.el --- Custom face theme for Emacs  -*-coding: utf-8 -*-

;; Copyright (C) 2016 Cezary Stankiewicz

;; Author: Cezary Stankiewicz (concat "c.stankiewicz" "@" "wlv.ac.uk")

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My default theme derived from wombat theme.

;;; Code:

(deftheme cs-default
  "Created 2016-09-26.")

;;; Faces

(custom-theme-set-faces
 'cs-default
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :width normal :height 90 :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "#f6f3e8" :background "#242424" :stipple nil :inherit nil))))
 '(cursor ((t (:background "#656565"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((t (:family "Sans Serif"))))
 '(escape-glyph ((t (:weight bold :foreground "#ddaa6f"))))
 '(minibuffer-prompt ((t (:foreground "#e5786d"))))
 '(highlight ((t (:underline (:color foreground-color :style line) :foreground "#ffffff" :background "#454545"))))
 '(region ((t (:foreground "#f6f3e8" :background "#444444"))))
 '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
 '(secondary-selection ((t (:foreground "#f6f3e8" :background "#333366"))))
 '(trailing-whitespace ((((class color) (background light)) (:background "red1")) (((class color) (background dark)) (:background "red1")) (t (:inverse-video t))))
 '(font-lock-builtin-face ((t (:foreground "#e5786d"))))
 '(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:foreground "#99968b"))))
 '(font-lock-constant-face ((t (:foreground "#e5786d"))))
 '(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
 '(font-lock-function-name-face ((t (:foreground "#cae682"))))
 '(font-lock-keyword-face ((t (:weight bold :foreground "#8ac6f2"))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "#95e454"))))
 '(font-lock-type-face ((t (:weight bold :foreground "#92a65e"))))
 '(font-lock-variable-name-face ((t (:foreground "#cae682"))))
 '(font-lock-warning-face ((t (:foreground "#ccaa8f"))))
 '(button ((t (:foreground "#f6f3e8" :background "#333333"))))
 '(link ((t (:underline (:color foreground-color :style line) :foreground "#8ac6f2"))))
 '(link-visited ((t (:underline (:color foreground-color :style line) :foreground "#e5786d"))))
 '(fringe ((t (:background "#303030"))))
 '(header-line ((t (:foreground "#e7f6da" :background "#303030"))))
 '(tooltip ((((class color)) (:inherit (variable-pitch) :foreground "black" :background "lightyellow")) (t (:inherit (variable-pitch)))))
 '(mode-line ((t (:foreground "#f6f3e8" :background "#444444"))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "grey40" :style released-button))) (t (:inherit (highlight)))))
 '(mode-line-inactive ((t (:foreground "#857b6f" :background "#444444"))))
 '(isearch ((t (:foreground "#857b6f" :background "#343434"))))
 '(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "RosyBrown1")) (((class color) (min-colors 88) (background dark)) (:background "red4")) (((class color) (min-colors 16)) (:background "red")) (((class color) (min-colors 8)) (:background "red")) (((class color grayscale)) (:foreground "grey")) (t (:inverse-video t))))
 '(lazy-highlight ((t (:foreground "#a0a8b0" :background "#384048"))))
 '(match ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "RoyalBlue3")) (((class color) (min-colors 8) (background light)) (:foreground "black" :background "yellow")) (((class color) (min-colors 8) (background dark)) (:foreground "white" :background "blue")) (((type tty) (class mono)) (:inverse-video t)) (t (:background "gray"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))
 ;; Org mode faces
 '(org-agenda-calendar-event ((t (:inherit (default)))))
 '(org-agenda-calendar-sexp ((t (:inherit (default)))))
 '(org-agenda-clocking ((t (:inherit (secondary-selection)))))
 '(org-agenda-column-dateline ((t (:inherit (org-column)))))
 '(org-agenda-current-time ((t (:inherit (org-time-grid)))))
 '(org-agenda-date ((t (:inherit (org-agenda-structure)))))
 '(org-agenda-date-today ((t (:italic t :weight bold :inherit (org-agenda-date)))))
 '(org-agenda-date-weekend ((t (:weight bold :inherit (org-agenda-date)))))
 '(org-agenda-diary ((t (:inherit (default)))))
 '(org-agenda-dimmed-todo-face ((((background light)) (:foreground "grey50")) (((background dark)) (:foreground "grey50"))))
 '(org-agenda-done ((((class color) (min-colors 16) (background light)) (:foreground "ForestGreen")) (((class color) (min-colors 16) (background dark)) (:foreground "PaleGreen")) (((class color) (min-colors 8)) (:foreground "green")) (t (:bold nil))))
 '(org-agenda-filter-category ((t (:inherit (mode-line)))))
 '(org-agenda-filter-regexp ((t (:inherit (mode-line)))))
 '(org-agenda-filter-tags ((t (:inherit (mode-line)))))
 '(org-agenda-restriction-lock ((((class color) (min-colors 88) (background light)) (:background "#eeeeee")) (((class color) (min-colors 88) (background dark)) (:background "#1C1C1C")) (((class color) (min-colors 16) (background light)) (:background "#eeeeee")) (((class color) (min-colors 16) (background dark)) (:background "#1C1C1C")) (((class color) (min-colors 8)) (:foreground "black" :background "cyan")) (t (:inverse-video t))))
 '(org-agenda-structure ((((class color) (min-colors 88) (background light)) (:foreground "Blue1")) (((class color) (min-colors 88) (background dark)) (:foreground "LightSkyBlue")) (((class color) (min-colors 16) (background light)) (:foreground "Blue")) (((class color) (min-colors 16) (background dark)) (:foreground "LightSkyBlue")) (((class color) (min-colors 8)) (:bold t :foreground "blue")) (t (:bold t))))
 '(org-archived ((t (:inherit (shadow)))))
 '(org-block ((t (:inherit (shadow)))))
 '(org-block-begin-line ((t (:inherit (org-meta-line)))))
 '(org-block-end-line ((t (:inherit (org-block-begin-line)))))
 '(org-checkbox ((t (:inherit (bold)))))
 '(org-checkbox-statistics-done ((t (:inherit (org-done)))))
 '(org-checkbox-statistics-todo ((t (:inherit (org-todo)))))
 '(org-clock-overlay ((((class color) (min-colors 88) (background light)) (:foreground "black" :background "LightGray")) (((class color) (min-colors 88) (background dark)) (:foreground "white" :background "SkyBlue4")) (((class color) (min-colors 16) (background light)) (:foreground "black" :background "gray")) (((class color) (min-colors 16) (background dark)) (:foreground "white" :background "SkyBlue4")) (((class color) (min-colors 8)) (:foreground "black" :background "cyan")) (t (:inverse-video t))))
 '(org-code ((t (:inherit (shadow)))))
 '(org-column ((((class color) (min-colors 16) (background light)) (:underline nil :strike-through nil :slant normal :weight normal :background "grey90")) (((class color) (min-colors 16) (background dark)) (:underline nil :strike-through nil :slant normal :weight normal :background "grey30")) (((class color) (min-colors 8)) (:underline nil :strike-through nil :slant normal :weight normal :foreground "black" :background "cyan")) (t (:inverse-video t))))
 '(org-column-title ((((class color) (min-colors 16) (background light)) (:weight bold :underline (:color foreground-color :style line) :background "grey90")) (((class color) (min-colors 16) (background dark)) (:weight bold :underline (:color foreground-color :style line) :background "grey30")) (((class color) (min-colors 8)) (:weight bold :underline (:color foreground-color :style line) :foreground "black" :background "cyan")) (t (:inverse-video t))))
 '(org-date ((((class color) (background light)) (:underline (:color foreground-color :style line) :foreground "Purple")) (((class color) (background dark)) (:underline (:color foreground-color :style line) :foreground "Cyan")) (t (:underline (:color foreground-color :style line)))))
 '(org-date-selected ((((class color) (min-colors 16) (background light)) (:inverse-video t :foreground "Red1")) (((class color) (min-colors 16) (background dark)) (:inverse-video t :foreground "Pink")) (((class color) (min-colors 8) (background light)) (:inverse-video t :foreground "red")) (((class color) (min-colors 8) (background dark)) (:inverse-video t :foreground "red")) (t (:inverse-video t))))
 '(org-default ((t (:inherit (default)))))
 '(org-document-info ((((class color) (background light)) (:foreground "midnight blue")) (((class color) (background dark)) (:foreground "pale turquoise")) (t nil)))
 '(org-document-info-keyword ((t (:inherit (shadow)))))
 '(org-document-title ((((class color) (background light)) (:weight bold :foreground "midnight blue")) (((class color) (background dark)) (:weight bold :foreground "pale turquoise")) (t (:weight bold))))
 '(org-done ((((class color) (min-colors 16) (background light)) (:bold t :foreground "ForestGreen")) (((class color) (min-colors 16) (background dark)) (:bold t :foreground "PaleGreen")) (((class color) (min-colors 8)) (:foreground "green")) (t (:bold t))))
 '(org-drawer ((((class color) (min-colors 88) (background light)) (:foreground "Blue1")) (((class color) (min-colors 88) (background dark)) (:foreground "LightSkyBlue")) (((class color) (min-colors 16) (background light)) (:foreground "Blue")) (((class color) (min-colors 16) (background dark)) (:foreground "LightSkyBlue")) (((class color) (min-colors 8)) (:bold t :foreground "blue")) (t (:bold t))))
 '(org-ellipsis ((((class color) (background light)) (:underline (:color foreground-color :style line) :foreground "DarkGoldenrod")) (((class color) (background dark)) (:underline (:color foreground-color :style line) :foreground "LightGoldenrod")) (t (:strike-through t))))
 '(org-footnote ((((class color) (background light)) (:underline (:color foreground-color :style line) :foreground "Purple")) (((class color) (background dark)) (:underline (:color foreground-color :style line) :foreground "Cyan")) (t (:underline (:color foreground-color :style line)))))
 '(org-formula ((((class color) (min-colors 88) (background light)) (:foreground "Firebrick")) (((class color) (min-colors 88) (background dark)) (:foreground "chocolate1")) (((class color) (min-colors 8) (background light)) (:foreground "red")) (((class color) (min-colors 8) (background dark)) (:foreground "red")) (t (:italic t :bold t))))
 '(org-headline-done ((((class color) (min-colors 16) (background light)) (:foreground "RosyBrown")) (((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon")) (((class color) (min-colors 8) (background light)) (:bold nil))))
 '(org-hide ((t (:foreground "#242424"))))
 '(org-indent ((t (:inherit (org-hide)))))
 '(org-latex-and-related ((((class grayscale) (background light)) (:inherit (underline) :foreground "DimGray")) (((class grayscale) (background dark)) (:inherit (underline) :foreground "LightGray")) (((class color) (background light)) (:foreground "SaddleBrown")) (((class color) (background dark)) (:foreground "burlywood")) (t (:inherit (underline)))))
 '(org-level-1 ((t (:inherit (outline-1)))))
 '(org-level-2 ((t (:inherit (outline-2)))))
 '(org-level-3 ((t (:inherit (outline-3)))))
 '(org-level-4 ((t (:inherit (outline-4)))))
 '(org-level-5 ((t (:inherit (outline-5)))))
 '(org-level-6 ((t (:inherit (outline-6)))))
 '(org-level-7 ((t (:inherit (outline-7)))))
 '(org-level-8 ((t (:inherit (outline-8)))))
 '(org-link ((t (:inherit (link)))))
 '(org-list-dt ((t (:bold t))))
 '(org-macro ((t (:inherit (org-latex-and-related)))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face)))))
 '(org-mode-line-clock ((t (:inherit (mode-line)))))
 '(org-mode-line-clock-overrun ((t (:background "red" :inherit (mode-line)))))
 '(org-priority ((t (:inherit (font-lock-keyword-face)))))
 '(org-property-value ((t nil)))
 '(org-quote ((t (:inherit (org-block)))))
 '(org-scheduled ((((class color) (min-colors 88) (background light)) (:foreground "DarkGreen")) (((class color) (min-colors 88) (background dark)) (:foreground "PaleGreen")) (((class color) (min-colors 8)) (:foreground "green")) (t (:italic t :bold t))))
 '(org-scheduled-previously ((((class color) (min-colors 88) (background light)) (:foreground "Firebrick")) (((class color) (min-colors 88) (background dark)) (:foreground "chocolate1")) (((class color) (min-colors 8) (background light)) (:foreground "red")) (((class color) (min-colors 8) (background dark)) (:bold t :foreground "red")) (t (:bold t))))
 '(org-scheduled-today ((((class color) (min-colors 88) (background light)) (:foreground "DarkGreen")) (((class color) (min-colors 88) (background dark)) (:foreground "PaleGreen")) (((class color) (min-colors 8)) (:foreground "green")) (t (:italic t :bold t))))
 '(org-sexp-date ((((class color) (background light)) (:foreground "Purple")) (((class color) (background dark)) (:foreground "Cyan")) (t (:underline (:color foreground-color :style line)))))
 '(org-special-keyword ((t (:inherit (font-lock-keyword-face)))))
 '(org-table ((((class color) (min-colors 88) (background light)) (:foreground "Blue1")) (((class color) (min-colors 88) (background dark)) (:foreground "LightSkyBlue")) (((class color) (min-colors 16) (background light)) (:foreground "Blue")) (((class color) (min-colors 16) (background dark)) (:foreground "LightSkyBlue")) (((class color) (min-colors 8) (background light)) (:foreground "blue")) (((class color) (min-colors 8) (background dark)) nil)))
 '(org-tag ((t (:bold t))))
 '(org-tag-group ((t (:inherit (org-tag)))))
 '(org-target ((((class color) (background light)) (:underline (:color foreground-color :style line))) (((class color) (background dark)) (:underline (:color foreground-color :style line))) (t (:underline (:color foreground-color :style line)))))
 '(org-time-grid ((((class color) (min-colors 16) (background light)) (:foreground "DarkGoldenrod")) (((class color) (min-colors 16) (background dark)) (:foreground "LightGoldenrod")) (((class color) (min-colors 8)) (:weight light :foreground "yellow"))))
 '(org-todo ((((class color) (min-colors 16) (background light)) (:bold t :foreground "Red1")) (((class color) (min-colors 16) (background dark)) (:bold t :foreground "Pink")) (((class color) (min-colors 8) (background light)) (:bold t :foreground "red")) (((class color) (min-colors 8) (background dark)) (:bold t :foreground "red")) (t (:bold t :inverse-video t))))
 '(org-upcoming-deadline ((((class color) (min-colors 88) (background light)) (:foreground "Firebrick")) (((class color) (min-colors 88) (background dark)) (:foreground "chocolate1")) (((class color) (min-colors 8) (background light)) (:foreground "red")) (((class color) (min-colors 8) (background dark)) (:bold t :foreground "red")) (t (:bold t))))
 '(org-verbatim ((t (:inherit (shadow)))))
 '(org-verse ((t (:inherit (org-block)))))
 '(org-warning ((t (:inherit (font-lock-warning-face))))))

(provide-theme 'cs-default)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; cs-default-theme.el ends here
