;;; para-test.el --- Tests for Para -*- lexical-binding: t; -*-
;;
;; Copyright © 2015 Mark Karpov <markkarpov@openmailbox.org>
;;
;; Author: Mark Karpov <markkarpov@openmailbox.org>
;; URL: https://github.com/para
;;
;; This file is not part of GNU Emacs.
;;
;; Para is free software: you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any
;; later version.
;;
;; Para is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'undercover)

(undercover "para.el")

(require 'cl-lib)
(require 'para)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicates / manipulation of S-expressions' internal representation

;; TODO


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Location / textual representation of S-expressions

(defun para-find-sexps-check (source pairs result &rest optimized-results)
  "Check that ‘para--find-sexps’ returns expected results.

SOURCE should be a string representing textual snippet with
S-expressions.  It will be inserted in a brand-new buffer where
‘text-mode’ is enabled.  The star symbol ★ in SOURCE denotes
position of point.  PAIRS is collection of active pairs to be
passed to ‘para--find-sexps’.  RESULT is collection of
S-expressions that should be detected without optimization
hints (i.e. all S-expressions there).  OPTIMIZED-RESULTS is
arbitrary number of lists where first element is list with four
elements — optimization hints as per docs of ‘para--find-sexps’,
and the rest is collection of S-expressions that should be
returned when those optimization hints are used."
  (let ((pos (cl-position ?★ source)))
    (if pos
        (dolist (item
                 (cons
                  (cons (lambda ()
                          (let ((para--ignore-hints t))
                            (para--find-sexps pairs 0 0 0 0)))
                        result)
                  (mapcar (lambda (x)
                            (cons (lambda ()
                                    (apply #'para--find-sexps pairs (car x)))
                                  (cdr x)))
                          optimized-results)))
          (cl-destructuring-bind (a . r) item
            (with-temp-buffer
              (text-mode)
              (insert (remove ?★ source))
              (newline)
              (goto-char pos)
              (should (equal (funcall a) r)))))
      (error "Test snippet must contain point position as ★"))))

(defmacro para-find-sexps-test (name source pairs result &rest optimized-results)
  "Define ert test for ‘para--find-sexps’.

The helper automatically extracts prefix from NAME and uses it to
name result test.  If the prefix (everything before first space)
contains asterisk *, this test is allowed to fail.  It trims
empty lines from the beginning of SOURCE, which should contain
textual snippet with S-expressions and ★ denoting position of
point.  PAIRS is collection of defined pairs to pass to
‘para--find-sexps’ function.  RESULT is full collection of
S-expressions ‘para--find-sexps’ should return when
‘para--ignore-hints’ is not NIL.  OPTIMIZED-RESULTS is arbitrary
number of lists where first element is list with four elements —
optimization hints as per docs of ‘para--find-sexps’, and the
rest is collection of S-expressions that should be returned when
those optimization hints are used.

PAIRS, RESULT, and OPTIMIZED-RESULTS should not be quoted, the
macro does this for you."
  (declare (indent defun))
  (let ((split-pos (cl-position 32 name)))
    (cl-destructuring-bind (test-name allow-failure doc-string)
        (if split-pos
            (let ((raw-prefix (substring name 0 split-pos)))
              (list (intern
                     (concat "para--find-sexps-check-"
                             (remove ?* raw-prefix)))
                    (cl-find ?* raw-prefix)
                    (substring name (1+ split-pos))))
          (list 'para--find-sexps-check-fixme
                nil
                name))
      `(ert-deftest ,test-name ()
         ,doc-string
         :expected-result
         ,(if allow-failure :failed :passed)
         (para-find-sexps-check
          ,(string-trim-left source)
          ',pairs
          ',result
          ,@(mapcar (lambda (x) (list 'quote x))
                    optimized-results))))))

(para-find-sexps-test "1 Doesn't do anything funny in empty buffers""
★"
  (("(" . ")") ("[" . "]"))
  nil
  ((0 0 0 1))
  ((0 0 1 0))
  ((0 1 0 0))
  ((1 0 0 0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API for defining / removal of pairs

;; TODO


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive aspects of editing and minor mode

;; TODO


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing commands

;; TODO

(provide 'para-test)

;;; para-test.el ends here
