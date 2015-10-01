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
  (para--sexp sexp
    (list os oe)))

(defun para--inner-boundaries (sexp)
  "Return list of two elements representing inner boundaries of SEXP."
  (para--sexp sexp
    (list is ie)))

;; S-expressions in context of this package can be simple or
;; compound. Simple expressions are those with equal OS and IS, IE and
;; OE. This means that they are formed by simple symbols without any
;; paired character sequences around them. For compound S-expressions
;; opening characters sequence in placed between OS and IS, closing
;; character sequences is placed between IE and OE.

(defun para--simple-p (sexp)
  "Test whether given SEXP is compound."
  (para--sexp sexp
    (and (eql os is)
         (eql ie oe))))

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
    (cl-destructuring-bind (ps pe) (para--inner-boundaries parent)
      (and (>= cs ps)
           (<= ce pe)))))

;; Sometimes it's convenient to be able to tell if particular position is
;; inside of given S-expression.

(defun para--point-in (point sexp)
  "Test whether POINT is inside of SEXP S-expression."
  (para--sexp sexp
    (and (>= point os)
         (<= point oe))))

;; Finally, it could be of interest if given S-expression is deep and yet
;; contains given point inside.

(defun para--around-point (point sexp)
  "Test whether POINT is inside of deep S-expression SEXP."
  (and (para--deep-p sexp)
       (para--point-in point sexp)))

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

(defun para--find-sexps (pairs backward forward n-deep)
  "Find S-exressions around point.

PAIRS is a list of cons that represent opening and closing
character sequences of active pairs.

Other arguments of the function are optimization hints.  The
hints help limit amount of work that needs to be done by the
function.

BACKWARD tells the system how many S-expressions before point is
expected to be found.  FORWARD does the same for S-expressions
after point.  N-DEEP arguments specifies how many embracing
S-expressions is required.

The function will return at lest that many S-expressions to
satisfy requirements expressed in the hints.  It however may
return more S-expressions than expected and users of the function
should deal with that filtering returned data.

Returned collection of S-expressions is guaranteed to be in
normalized order."
  ;; TODO write the function
  (ignore (list pairs backward forward n-deep)))

(defvar para--searching-function #'para--find-sexps
  "Function to be used to find S-expressions around the point.

The function is called with four arguments, see description of
`para--find-sexps' for more information about their meaning and
expected behavior of the function.

This function should not move point or have any other
side-effects.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API for defining / removal of pairs

;; This section describes how pairs are defined and undefined. The package
;; allows to define global pairs that work everywhere except for some modes
;; (blacklisted modes) and pairs that work selectively, only in specified
;; major modes. When `para-mode' is activated exact set of active pairs is
;; formed and stored in `para--active-pairs' variable. This variable is then
;; used by most other commands.

(defvar para--active-pairs nil
  "List of cons representing active pairs in current buffer.

This list is recalculated by `para--activate-pairs', normally
every time `para-mode' is activated.")

(make-variable-buffer-local 'para--active-pairs)

(defvar para--global-pairs nil
  "List representing defined global pairs.

Every element of the list is list where the first element is
representation of pair (a cons) and the rest is collection of
blacklisted modes.")

(defvar para--local-pairs nil
  "List representing defined local pairs.

Every element of the list is list where the first element is
representation of pair (a cons) and the rest is collection of
symbols — names of major modes where this pair is active.")

;;;###autoload
(defun para-add-global-pair (start end &rest blacklist)
  "Define global pair starting with START, ending with END.

This will be active everywhere except for modes in BLACKLIST.

If START is prefix of some existing global pair, error will be
signalled.  If exactly the same pair already exists, its
blacklist will be merged with BLACKLIST."
  ;; FIXME make it conform to the description
  (push (cons (cons start end) blacklist)
        para--global-pairs))

;;;###autoload
(defun para-add-local-pair (start end &rest modes)
  "Define local pair starting with START, ending with END.

This will be active only in modes listed in MODES.

If START is prefix of some existing local pair…
XXX Are these all “dangerous” cases? Think about it."
  ;; FIXME make it conform to the description
  (push (cons (cons start end) modes)
        para--local-pairs))

;;;###autoload
(defun para-remove-global-pair (start end)
  "Remove global pair staring with START and ending with END."
  (setq para--global-pairs
        (cl-delete (cons start end)
                   para--global-pairs
                   :key #'car
                   :test #'equal)))

;;;###autoload
(defun para-remove-local-pair (start end)
  "Remove local pair starting with START and ending with END."
  (setq para--local-pairs
        (cl-delete (cons start end)
                   para--local-pairs
                   :key #'car
                   :test #'equal)))

(defun para--activate-pairs (enable)
  "Populate `para--active-pairs' for current buffer.

If ENABLE has non-NIL value, set value of `para--active-pairs' to
list of active pairs.  Otherwise set it to NIL."
  (setq
   para--active-pairs
   (when enable
     (mapcar
      #'car
      (cl-concatenate
       'list
       (cl-remove-if
        (apply-partially #'cl-find major-mode)
        para--global-pairs
        :key #'cdr)
       (cl-remove-if-not
        (apply-partially #'cl-find major-mode)
        para--local-pairs
        :key #'cdr))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive aspects of editing and minor mode

;; This section handles interactive aspects of editing: auto-insertion and
;; auto-deletion of pairs, wrapping. The section also defines minor mode
;; `para-mode'.

(defcustom para-active-in-minibuffer t
  "Whether to activate `para-mode' in minibuffer."
  :tag  "Enable Para Mode in Minibuffer"
  :type 'boolean)

;;;###autoload
(defcustom para-excluded-modes nil
  "List of major modes for which `para-mode' should not be activated.

This variable is considered when Para is enabled globally via
`para-global-mode'."
  :tag  "Excluded Modes"
  :type '(repeat :tag "Major modes to exclude" symbol))

;;;###autoload
(define-minor-mode para-mode
  "Toggle `para-mode' minor mode.

With a prefix argument ARG, enable `para-mode' if ARG is
positive, and disable it otherwise"
  nil " Para" para-mode-map
  (para--activate-pairs para-mode))

(defun para--maybe-activate ()
  "Activate `para-mode' in current buffer.

Value of `para-active-in-minibuffer' control whether to activate
the mode in minibuffer."
  (unless (or (and (minibufferp)
                   (not para-active-in-minibuffer))
              (member major-mode para-excluded-modes))
    (para-mode 1)))

;;;###autoload
(define-globalized-minor-mode para-global-mode
  para-mode
  para--maybe-activate)

(defmacro para--with-sexp (backward forward n-deep predicate &rest body)
  "Bind symbol `sexps' to found S-expressions around point.

BACKWARD, FORWARD, and N-DEEP are hints used to find
S-expressions around point.  You can read more about meaning of
these parameters in documentation of `para-find-sexps'.

Note that combination of `para--searching-function' and
`para--active-pairs' is used to find S-expressions.

Unless PREDICATE it's NIL, it is used to filter returned
collection of S-expressions, only S-expressions satisfying the
predicate get through.

Once symbol `sexps' is bound, forms in BODY are evaluated in
order inside the modified lexical environment.  If `para-mode' is
not active or no S-expressions found nothing happens."
  (declare (indent defun))
  `(when para-mode
     (let ((sexps
            (cl-remove-if-not
             (or ,predicate (lambda (_x) t))
             (funcall para--searching-function
                      para--active-pairs
                      ,backward
                      ,forward
                      ,n-deep))))
       (when sexps
         ,@body))))

;; TODO Insertion of closing pair.
;; TODO Automatic deletion of pairs.
;; TODO Wrapping, See also `delete-selection-pre-hook'.

;; Everything should be done via `post-command-hook'. This is because some
;; characters are self-inserting, some of them are inserted via `insert' by
;; some modes, etc. `post-command-hook' should be the point where we can
;; catch everything.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing commands

;; Collection of basic editing primitives and cursor movements.

;; https://ebzzry.github.io/emacs-pairs.html

;;;###autoload
(defun para-beginning-of-sexp ()
  "Jump to beginning of the S-expression the point is in."
  (interactive)
  (para--with-sexp 0 0 1
    (apply-partially #'para--around-point (point))
    (para--sexp (car sexps)
      (goto-char is))))

;;;###autoload
(defun para-end-of-sexp ()
  "Jump to end of the S-expression the point is in."
  (interactive)
  (para--with-sexp 0 0 1
    (apply-partially #'para--around-point (point))
    (para--sexp (car sexps)
      (goto-char ie))))

;; (defun para-down-sexp (&optional arg)
;;   "Move point forward down one level of S-expression.

;; With ARG, do this that many times.  A negative argument -N means
;; move backward but still go down a level."
;;   (para--with-sexp )) ;; ???

;; para-up-sexp
;; para-backward-down-sexp
;; para-backward-up-sexp
;; para-forward-sexp
;; para-backward-sexp
;; para-next-sexp
;; para-previous-sexp
;; para-backward-unwrap-sexp
;; para-unwrap-sexp
;; para-forward-slurp-sexp
;; para-forward-barf-sexp
;; para-backward-slurp-sexp
;; para-backward-barf-sexp
;; para-transpose-sexp
;; para-kill-sexp
;; para-kill-hybrid-sexp
;; para-select-next-sexp
;; para-select-previous-sexp


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default configuration

;; Here we have default configuration for all supported out-of-box
;; modes. The decision to keep everything in one place and inside of main
;; source file has been made because I want to provide decent editing
;; experience by default without requiring any additional effort from
;; user. People with special requirements should edit this configuration
;; with help of `para-add-global-pair', `para-add-local-pair',
;; `para-remove-global-pair', and `para-remove-local-pair'.
;;
;; If you're interested in addition of new pairs to the package, this is
;; where to put your definitions:

;; global pairs

(para-add-global-pair "(" ")")
(para-add-global-pair "\"" "\"")
;; etc.

;; emacs-lisp-mode

(para-add-local-pair "\\\"" "\\\"" 'emacs-lisp-mode) ; for testing…

(provide 'para)

;;; para.el ends here
