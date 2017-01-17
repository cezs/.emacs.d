;;; cs-awker-mm.el --- Minor mode for editing column based data with awk.

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

(defcustom cs-awker-new-buffer t
  "Output results in new buffer."
  :group 'cs-awker
  :type 'boolean)

(defcustom cs-awker-current-buffer nil
  "Replace current buffer content."
  :group 'cs-awker
  :type 'boolean)

(defvar cs-awker-mm-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-#") 'cs-awker-awk-print-col-op-on-region)
    map)
  "Keymap for Cs-Awker.")

(easy-menu-define my-menu cs-awker-mm-map "Cs-Awker menu"
  `("Cs-Awker"
     ["Use new buffer"
      (progn (setq cs-awker-new-buffer t)
             (setq cs-awker-current-buffer nil)
             (message "Turned %s printing to new buffer"
                      (if cs-awker-new-buffer "on" "off")))
      :style radio  :selected cs-awker-new-buffer :help "Use new buffer"]
     ["Use current buffer"
      (progn (setq cs-awker-new-buffer nil)
             (setq cs-awker-current-buffer t)
             (message "Turned %s printing to current buffer"
                      (if cs-awker-current-buffer "on" "off")))
      :style radio  :selected cs-awker-current-buffer :help "Use current buffer"]
     "---"
     ["Print column"
      cs-awker-awk-print-col-op-on-region]
     ["Print unique values of column"
      cs-awker-awk-print-col-op-uniq-on-region]
     ["Print column's total"
      cs-awker-awk-print-col-op-sum-on-region]))

(defun cs-awker-awk-print-col-global-on-region (start end cmmd)
  "Execute awk command `cmmd' on either whole buffer or just selected region."
  (let ((buff (current-buffer)))
    (when cs-awker-new-buffer
      (find-file "*Cs-Awker Results*")
      (switch-to-prev-buffer)
      (setq buff (find-buffer-visiting "*Cs-Awker Results*")))
    (shell-command-on-region
     start
     end
     cmmd
     buff
     nil
     "*Cs-Awker Error Buffer*"
     t)))

(defun cs-awker-awk-print-col-op-on-region (start end)
  "Print column. Works on either whole buffer or selected region."
  (interactive "r")
  (let* ((colnum (read-string "Column number: "))
         (cmmd (concat "awk '{print $" colnum "}'")))
    (if mark-active
        (cs-awker-awk-print-col-global-on-region start end cmmd)
      (cs-awker-awk-print-col-global-on-region (point-min) (point-max) cmmd))))

(defun cs-awker-awk-print-col-op-uniq-on-region (start end)
  "Print unique values of column. Works on either whole buffer or selected 
  region"
  (interactive "r")
  (let* ((colnum (read-string "Column number: "))
         (cmmd (concat "awk '{ print $" colnum "}' | sort | uniq")))
    (if mark-active
        (cs-awker-awk-print-col-global-on-region start end cmmd)
      (cs-awker-awk-print-col-global-on-region (point-min) (point-max) cmmd))))

(defun cs-awker-awk-print-col-op-sum-on-region (start end)
  "Print column's total. Works on either whole buffer or selected region."
  (interactive "r")
  (let* ((colnum (read-string "Column number: "))
         (cmmd (concat "awk '{ sum += $" colnum "} END {print sum}' ")))
    (if mark-active
        (cs-awker-awk-print-col-global-on-region start end cmmd)
      (cs-awker-awk-print-col-global-on-region (point-min) (point-max) cmmd))))

(defun cs-awker-disable ())

(defun cs-awker-enable ())

;;;###autoload
(define-minor-mode cs-awker-mm
  "What it does?"
  :init-value nil
  :lighter " Cs-Awker"
  :keymap cs-awker-mm-map

  (if cs-awker-mm
      (cs-awker-enable)
    (cs-awker-disable)))

(provide 'cs-awker-mm)

;;; cs-awker-mm.el ends here
