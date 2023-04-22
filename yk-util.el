;;; yk-util.el --- My utility functions      -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Yusuke Kitamura

;; Author: Yusuke Kitamura <ymyk6602@gmail.com>
;; Keywords: extensions
;; URL:
;; Package-Requires: ((emacs "25.1") (dash "2.18.1"))
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; My utility functions which make it easier to write elisp.

;;; Code:
(require 'cl-lib)
(require 'dash)

(defgroup yk nil
  "My utility functions"
  :group 'convenience)

(defun yk/add-to-list-multiple (list-var elements &optional append compare-fn)
  "Add each element in ELEMENTS to the value of LIST-VAR if it isn't there yet.
LIST-VAR is symbol of the list to which ELEMENTS is added.
ELEMENTS must be list.
If ELEMENTS is added, it is added at the beginning of the list."
  (cl-assert (listp elements))
  (--map (add-to-list list-var it append compare-fn) elements))

(defun yk/read_shell_file_to_set_envvar (file)
    "Read FILE and set environment variables."
    (let ((lines (with-temp-buffer
                   (insert-file-contents file)
                   (split-string (buffer-string) "\\\n"))))
      (-map (lambda (line)
              (when (string-match "export \\(.*\\)=\\(.*\\)" line)
                (let ((var (match-string 1 line))
                      (val (match-string 2 line)))
                  (setenv var val)
                  ;; (message "Set environment variable %s to %s" var val)
                  )))
            lines)))


(defvar yk/compress-file-suffixes
  '(("\\.tar\\.gz\\'"  "gzip -dc %i | tar -xf -")
    ("\\.tar\\.xz\\'"  "xz -dc %i | tar -xf -")
    ("\\.tgz\\'"  "gzip -dc %i | tar -xf -")
    ("\\.gz\\'"  "gunzip")
    ("\\.lz\\'"  "lzip -d")
    ("\\.Z\\'"  "uncompress")
    ("\\.z\\'"  "gunzip")
    ("\\.dz\\'"  "dictunzip")
    ("\\.tbz\\'" ".tar" "bunzip2")
    ("\\.bz2\\'"  "bunzip2")
    ("\\.xz\\'"  "unxz")
    ("\\.zip\\'"  "unzip -o -d %o %i")
    ("\\.tar\\.zst\\'" "unzstd -c %i | tar -xf -")
    ("\\.tzst\\'" "unzstd -c %i | tar -xf -")
    ("\\.zst\\'" "unzstd --rm")
    ("\\.7z\\'" "7z x -aoa -o%o %i")))

;; (defun yk/uncompress-file (file &optional output-directory)
;;   ""
;;   (let (suffix newname
;;         (suffixes yk/compress-file-suffixes))
;;     (while suffixes
;;       (if (string-match (car car suffixes) file)
;;           (setq suffix (car suffixes) suffixes nil)
;;         (setq suffixes (cdr suffixes))))
;;     (if suffix
;;         (setq newname (concat ))))
;;   )



(provide 'yk-util)
;;; yk-util.el ends here
