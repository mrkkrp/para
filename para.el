;;; para.el --- Clean package to deal with pairs effectively -*- lexical-binding: t; -*-
;;
;; Copyright © 2015 Mark Karpov <markkarpov@openmailbox.org>
;;
;; Author: Mark Karpov <markkarpov@openmailbox.org>
;; URL: https://github.com/mrkkrp/para
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: convenience text pair
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

;; This package allows to deal with pairs efficiently. It provides the
;; following:
;;
;; * Functions for navigation in and manipulation of S-expressions. These
;;   include:
;;
;;     * `para-beginning-of-sexp'
;;     * `para-end-of-sexp'
;;     * `para-down-sexp'
;;     * `para-up-sexp'
;;     * `para-backward-down-sexp'
;;     * `para-backward-up-sexp'
;;     * `para-forward-sexp'
;;     * `para-backward-sexp'
;;     * `para-next-sexp'
;;     * `para-previous-sexp'
;;     * `para-backward-unwrap-sexp'
;;     * `para-unwrap-sexp'
;;     * `para-forward-slurp-sexp'
;;     * `para-forward-barf-sexp'
;;     * `para-backward-slurp-sexp'
;;     * `para-backward-barf-sexp'
;;     * `para-transpose-sexp'
;;     * `para-kill-sexp'
;;     * `para-kill-hybrid-sexp'
;;     * `para-select-next-sexp'
;;     * `para-select-previous-sexp'
;;
;; * Automatic insertion of closing pair. Para *does not* skip closing pair
;;   if it already exists, it inserts closing pair if you press key
;;   corresponding to it. It's easy to get into the habit of moving cursor
;;   one character forward instead of trying to insert already inserted
;;   character. This feature makes the package simpler, this is one of aims
;;   of this project.
;;
;; * Automatic deletion of complete pairs. If cursor is placed behind whole
;;   pair and you press backspace, closing pair is deleted, as usual. If
;;   cursor is between paired characters and there is no text between them,
;;   only in that case deleting of opening pair will delete the closing pair
;;   as well. Otherwise opening pair is deleted as usual.
;;
;; * Wrapping of active region. If you have active region and enter paired
;;   character the region is wrapped and mark is deactivated. Due to certain
;;   confusion that this feature can lead to (if you prefer to delete
;;   selected text when new text is typed), only single-character pairs work
;;   this way.
;;
;; * Some pairs, like simple single quote which often serves as apostrophe
;;   and can be part of identifiers in some languages (Haskell), are handled
;;   intelligently. When it can be part of word or identifier it's not
;;   automatically paired. You can give this behavior to other pairs, since
;;   the package is quite flexible, but default configuration should be OK
;;   for 99% of users.
;;
;; * The package can work with multi-character pairs and pairs where opening
;;   and closing sequences are identical.
;;
;; However, Para is not a clone of Smartparens. It's much more clean and
;; minimalistic. My opinion is that packages for basic editing should be
;; simple, robust, and they should not ever let you down. Every such package
;; should be like a hammer. To achieve these goals Para adheres to the
;; following principles:
;;
;; * No state in your editing. Every combination of user's actions always
;;   result in the same thing.
;;
;; * No useless flashy things. Overlays in basic editing? Forget it.
;;
;; * Clear API for users. To define a pair you need to specify exactly two
;;   things: opening pair and closing pair. You can specify when this pair
;;   will be active too, but it's a different thing.
;;
;; * Clean design from the very beginning and a lot of tests covering
;;   *everything*.

;;; Code:

(require 'cl-lib)

(defgroup para nil
  "Clean package to deal with pairs effectively"
  :group  'editing
  :tag    "Para"
  :prefix "para-"
  :link   '(url-link :tag "GitHub" "https://github.com/mrkkrp/para"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicates / manipulation of S-expressions' internal representation

;; This section describes representation of various S-expressions in context
;; of this package and defines several predicates and conceptual
;; transforming functions.
;;
;; S-expression is represented as list containing exactly 4 elements:
;;
;;   (OS IS IE OE)
;;
;; where:
;;
;; * OS — “outer start” where the entire S-expression begins.
;; * IS — “inner start” where actual content begins.
;; * IE — “inner end” where actual content ends.
;; * OE — “outer end” where the entire S-expression ends.

(defmacro para--sexp (sexp &rest forms)
  "Bind symbols `os', `is', `ie', and `oe' to parts of SEXP.

Then evaluate FORMS in this context."
  (declare (indent 1))
  `(cl-destructuring-bind (os is ie oe) ,sexp
     (ignore (list os is ie oe))
     ,@forms))

(defun para--outer-boundaries (sexp)
  "Return list of two elements representing outer boundaries of SEXP."
  (para--sexp sexp (list os oe)))

(defun para--inner-boundaries (sexp)
  "Return list of two elements representing inner boundaries of SEXP."
  (para--sexp sexp (list is ie)))

;; S-expressions in context of this package can be simple or
;; compound. Simple expressions are those with equal OS and IS, IE and
;; OE. This means that they are formed by simple symbols without any
;; paired character sequences around them. For compound S-expressions
;; opening characters sequence in placed between OS and IS, closing
;; character sequences is placed between IE and OE.

(defun para--simple-p (sexp)
  "Test whether given SEXP is compound."
  (para--sexp sexp (and (eql os is) (eql ie oe))))

(defun para--compound-p (sexp)
  "Test whether given SEXP is compound."
  (not (para--simple-p sexp)))

;; Compound expressions can be balanced or unbalanced. Unbalanced
;; S-expression occurs when opening or closing character sequence is not
;; found. To represent unbalanced S-expression we put NIL on place of start
;; or end positions. Both outer and inner positions should be NIL when
;; unbalanced S-expression is represented.

(defun para--balanced-p (sexp)
  "Test whether given SEXP is balanced."
  (cl-every #'identity sexp))

(defun para--unbalanced-p (sexp)
  "Test whether given SEXP is unbalanced."
  (not (para--balanced-p sexp)))

;; Balanced compound S-expressions are called deep. “Deep” in our case only
;; means that the S-expression in question has some depth, i.e. it can be
;; traversed inward.

(defun para--deep-p (sexp)
  "Test whether given SEXP is deep."
  (and (para--compound-p sexp)
       (para--balanced-p sexp)))

;; Once we have defined notion of deep S-expressions we can tell if one
;; S-expression is placed inside of other.

(defun para--inside-of (child parent)
  "Test whether CHILD is inside of PARENT S-expression."
  (cl-destructuring-bind (cs ce) (para--outer-boundaries child)
    (cl-destructuring-bind (ps pe) (para--inner-boudaries parent)
      (and (>= cs ps)
           (<= ce pe)))))

;; We'll often work with collections of S-expressions, thus we need a notion
;; of normalized order for them. Normalized order is such order where every
;; next S-expression has greater value of position where it ends
;; (i.e. greater OE value). Since it's not possible for two S-expressions to
;; have the same value of OE, it's always clear which of them should go
;; first.

(defun para--normalize (sexps)
  "Return list including SEXPS in normalized order.

This is destructive function; it reuses storage of SEXPS if possible."
  (cl-sort sexps :key #'cl-fourth))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Location / textual representation of S-expressions

;; This section defines how text in buffer maps to internal representation
;; of S-expressions. Functions in this section thus define how S-expressions
;; are textually represented in context of this package and abstract the
;; procedure of S-expression recognition.
;;
;; Under no circumstances other parts of this software should attempt to
;; parse text in buffer, otherwise the design will be broken.

(provide 'para)

;;; para.el ends here
