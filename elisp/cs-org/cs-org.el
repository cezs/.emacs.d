;;; cs-org.el --- Utilities for Org mode.

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

;; Utilities for Org mode.

;;; Code:

(defun cs-org-babel-tangle-and-open ()
  "Tangle and open."
  (interactive)
  (if (eq major-mode 'org-mode)
      (progn (org-babel-tangle)
             (find-file (concat (substring (buffer-file-name) 0 -3) "el")))
    (error "Run it from buffer in org mode")))

;; TODO: Replace shell usage with elisp
(defun cs-org-delete-generated-files ()
  "Use find to clean directory from files generated with org."
  (interactive)
  (if (eq major-mode 'org-mode)
      (let* ((name-of-the-file
              (replace-regexp-in-string
               ".+/" ""
               (file-name-sans-extension buffer-file-name))
              ;; (substring (buffer-file-name) 0 -4)))
              (clean-non-org-files-cmd
               (concat "find " name-of-the-file
                       ".* -type f ! -name '*.org' -delete &")))
             (shell-command clean-non-org-files-cmd))
        (error "Run it from buffer in org mode"))))

;; TODO: Replace shell usage with more native code
(defun cs-org-delete-all-except-org-files ()
  "Use find to clean directory from files generated with org."
  (interactive)
  (if (eq major-mode 'org-mode)
      (shell-command "find . -type f ! -name '*.org' -delete &")
    (error "Run it from buffer in org mode")))

;; CHANGELOG:
;;
;; 2016-09-05
;; Changed:
;; ;; (interactive "nInstall package? (1 or 0): ")
;; to:
;; (interactive (list (yes-or-no-p "Install package? ")))
;;
;; 2016-09-02
;; Deprecated code: replacing shell usage with more native code
;; (let ((move-elisp-files (concat "mv " pkgname "*.el " dirname))
;;       (archive-package (concat "tar " "-cf " dirname ".tar " dirname))
;;       (clean-directory (concat "rm -r -d " dirname)))
;;   (shell-command move-elisp-files)
;;   (shell-command archive-package)
;;   (shell-command clean-directory))
;; were changed to:
;; (cs-move-all-elisp-files-to-dir dirname pkgname)
;; (shell-command (concat "tar " "-cf " dirname ".tar " dirname))
;; (delete-directory dirname t)
;;
;; 2016-08-30
;; This function previously used:
;; (shell-command (concat "cp " pkgname "*.el " dirname))
;; (shell-command (concat "find " pkgname "*.el -type f ! -name '*.org' -delete"))
;; Also changed:
;; (substring (buffer-file-name) 0 -4)))
;; to
;; (file-name-sans-extension buffer-file-name)))
(defun cs-org-file-to-emacs-package (x)
  "Use single Org file to prepare multi-file Emacs package archive.
After the creation, the package archive can be immediately installed with X.

Note: Sources inside org file have to be annotated as follows:
+begin_src <your-language> :tangle yes :exports code :comments link
  <your-code>
+end_src
where \":comments link\" is optional/required for detangling."
  (interactive (list (yes-or-no-p "Install package? ")))
  (if (eq major-mode 'org-mode)
      (let* ((pkgname
              (replace-regexp-in-string
               ".+/" ""
               (file-name-sans-extension buffer-file-name)))
             (datetime (format-time-string "%Y%m%d.%H%M%S"))
             (dirname (concat pkgname "-" datetime))
             (pkgmain (concat pkgname "-" "pkg.el")))
        (org-babel-tangle)
        (make-directory-internal dirname)
        (switch-to-buffer-other-window pkgmain)
        (erase-buffer)
        (insert (concat "(define-package \n"
                        "\t\"" pkgname "\" \n"
                        "\t\"" datetime "\" \n"
                        "\t\"" "This is " pkgname " package." "\" \n"
                        ";; Local Variables: \n"
                        ";; no-byte-compile: t \n"
                        ";; End: \n"
                        ")"))
        (write-file (concat dirname "/" pkgmain))
        (kill-this-buffer)
        (delete-window)
        (cs-move-all-elisp-files-starting-with-to-dir pkgname dirname)
        (shell-command (concat "tar " "-cf " dirname ".tar " dirname))
        (delete-directory dirname t)
        (if (eq x t)
            (package-install-file (concat default-directory dirname ".tar")))
        )
    (error "Run it from buffer in org mode")))

;; The following 4 are temporary: could not find any native command for moving files.

(defun cs-copy-all-elisp-files-starting-with-to-dir (p d)
  "Copy regexped elisp files starting with name P to directory D."
  (mapcar (lambda (n) (copy-file n d)) (directory-files "." t (concat p ".*\.el"))))

(defun cs-delete-all-elisp-files-starting-with (p)
  "Delete regexped elisp files starting with name P."
  (mapcar #'delete-file (directory-files "." nil (concat p ".*\.el"))))

(defun cs-move-all-elisp-files-starting-with-to-dir (p d)
  "Move regexped elisp files starting with name P to directory D."
  (cs-copy-all-elisp-files-starting-with-to-dir p d)
  (cs-delete-all-elisp-files-starting-with p))

(defun cs-clean-up (d)
  "Recursively remove directory D with files."
  (delete-directory d t))

(provide 'cs-org)

;;; cs-org.el ends here
