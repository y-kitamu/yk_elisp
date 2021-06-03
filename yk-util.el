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

(provide 'yk-util)
;;; yk-util.el ends here
