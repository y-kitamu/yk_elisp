;;; test-yk-util.el --- unit test for yk-util         -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Yusuke Kitamura

;; Author: Yusuke Kitamura <ymyk6602@gmail.com>
;; Keywords: extensions

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

;; unit test for yk-util

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'yk-util)

(ert-deftest yk/unittest-yk-add-to-list-multiple ()
  "Unit test of `yk/add-to-list-multiple'."
  (setq yk/test-src-list '("foo" "bar"))
  (let ((yk-add-list '("baz" "foo" "foobar")))
    (yk/add-to-list-multiple 'yk/test-src-list yk-add-list)
    (cl-mapcar
     (lambda (x y) (should (equal x y)))
     yk/test-src-list '("foobar" "baz" "foo" "bar")))
  (setq yk/test-src-list nil))

(ert-deftest yk/unittest-yk-add-to-list-bug ()
  "Unhappy path unit test of `yk/add-to-list-multiple'"
  :expected-result :failed
  (setq yk/test-src-list '("foo" "bar"))
  (yk/add-to-list-multiple 'yk/test-src-list "baz")
  (setq yk/test-src-list nil))

;;; test-yk-util.el ends here
