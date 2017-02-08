;;; cs-global.el --- Various functions to be used across all kind of buffers

;; Copyright (C) 2016-2017 Cezary Stankiewicz

;; Author: Cezary Stankiewicz (concat "c.stankiewicz" "@" "wlv.ac.uk")
;; Keywords: convenience

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

;; Various functions required for all kind of buffers

;;; Code:

(defun cs-kill-all-buffers ()
  "Kill all buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun cs-kill-this-buffer-volatile ()
  "Kill current buffer unconditionally."
  (interactive)
  (set-buffer-modified-p nil)
  (kill-this-buffer))

(defun cs-create-buffer ()
  "Create temporary buffer."
  (interactive)
  (switch-to-buffer (make-temp-name "*temp*")))

(defun net/update-tag-in-org-or-tex ()
  "Update tag in org or tex mode file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 1))
      (while (re-search-forward "\\tag{\\([0-9]+\\)}" nil t)
        (replace-match (format "%d" count) nil nil nil 1)
        (setq count (1+ count))))))

(defun cs-clear-terminal ()
  "Clear terminal."
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(defun cs-tangle-and-publish ()
  "Tangle and publish."
  (interactive)
  (org-babel-tangle)
  (org-publish-all))

(provide 'cs-global)

;;; cs-global.el ends here
