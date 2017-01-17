;;; cs-nixer.el --- Minor mode using various convenience linux application

;; Copyright (C) 2016-2017 Cezary Stankiewicz

;; Maintainer: Cezary Stankiewicz (concat "c.stankiewicz" "@" "wlv.ac.uk")
;; Keywords: convenience, data, files, matching, wp
;; Created: 15 August 2016

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;

;;; Commentary:
;;
;; Minor mode for editing column based data with awk.
;;

;;; Code:

;; ! Uncomment only if in Emacs-Lisp mode
;; (defgroup cs-nixer ((cs-nixer-new-buffer custom-variable)))


(defcustom cs-nixer-new-buffer t
  "Output results in new buffer"
  :group 'cs-nixer
  :type 'boolean)

(defcustom cs-nixer-current-buffer nil
  "Replace current buffer content"
  :group 'cs-nixer
  :type 'boolean)

(defvar cs-nixer-mm-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<C-f1>") 'cs-nixer-menu-bar-open)
    (define-key map (kbd "C->") 'cs-nixer-sh-cmd)
    (define-key map (kbd "C-<") 'cs-nixer-sh-cmd-region-replace-all)
    map)
  "Keymap for Cs-Nixer.")

(easy-menu-define my-menu cs-nixer-mm-map "Cs-Nixer menu"
  `("Cs-Nixer"
    ("Shell"
     ["Run command in shell"
      cs-nixer-sh-cmd]
     ["Run command in shell on whole region"
      cs-nixer-sh-cmd-region-replace-all]
     ["Run command in shell on whole region and print results in new buffer"
      cs-nixer-sh-cmd-region-new-all])
    "---"
    ("Emacs"
     ["Kill all buffers"
      cs-nixer-kill-all-buffers]
     ["Count lines"
      cs-nixer-count-lines]
     ["Count characters"
      cs-nixer-count-ch]
     "---"
     ("Org"
      ["Detangle and open"
       org-babel-detangle]))))

(defun cs-nixer-toggle-new-buffer ()
  "Activate the output in new buffer"
  (interactive)
  (if cs-nixer-new-buffer
      (setq cs-nixer-new-buffer nil)
    (setq cs-nixer-new-buffer t)))

(defun cs-nixer-next-file ()
  "Dired next file"
  (interactive)
  (dired-next-line 1))

(defun cs-nixer-dired-file-display ()
  "Display file at point"
  (interactive)
  (window-buffer
   (display-buffer
    (find-buffer-visiting
     (dired-file-name-at-point)))))

(defun cs-nixer-menu-bar-open ()
  "Open menu bar"
  (interactive)
  (menu-bar-open))

(defun cs-nixer-sh-cmd ()
  "Execute command in shell"
  (interactive)
  (shell-command (read-shell-command "> ")))

(defun cs-nixer-sh-cmd-region-replace-all ()
  "Execute command on region using shell"
  (interactive)
  (shell-command-on-region
   (point-min)
   (point-max)
   (read-shell-command "> ")
   (current-buffer)
   t
   "*Cs-Nixer Error Buffer*"
   t))

(defun cs-nixer-sh-cmd-region-new-all ()
  "Execute command on region using shell and output results to new buffer"
  (interactive)
  (find-file "*Cs-Nixer Results*")
  (switch-to-prev-buffer)
  (shell-command-on-region
   (point-min)
   (point-max)
   (read-shell-command "> ")
   (find-buffer-visiting "*Cs-Nixer Results*")
   nil
   "*Cs-Nixer Error Buffer*"
   t))

(defun cs-nixer-kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun cs-nixer-count-lines (start end)
  "Count lines"
  (interactive "r")
  (message "Counted %d lines in selected region."
           (count-lines start end)))

(defun cs-nixer-count-ch (start end)
  "Count characters"
  (interactive "r")
  (message "Counted %d characters in selected region." (- end start)))

(defun cs-nixer-disable ())

(defun cs-nixer-enable ())

;;;###autoload
(define-minor-mode cs-nixer-mm
  "What it does?"
  :init-value nil
  :lighter " Cs-Nixer"
  :keymap cs-nixer-mm-map

  (if cs-nixer-mm
      (cs-nixer-enable)
    (cs-nixer-disable)))

(provide 'cs-nixer-mm)

;;; cs-nixer-mm.el ends here
