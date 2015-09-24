;;; para.el --- Clean package to deal with pairs effectively -*- lexical-binding: t; -*-
;;
;; Copyright © 2015 Mark Karpov <markkarpov@openmailbox.org>
;;
;; Author: Mark Karpov <markkarpov@openmailbox.org>
;; URL: https://github.com/mrkkrp/para
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: convenience
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

;; This package replicates most part of functionality available in
;; Smartparens package. It provides the following things:
;;
;; * Functions for navigation in and manipulation of S-expressions. These
;;   include:
;;
;; * `para-beginning-of'
;; * `para-end-of'
;; * `para-down'
;; * `para-up'
;; * `para-backward-down'
;; * `para-backward-up'
;; * `para-forward'
;; * `para-backward'
;; * `para-next'
;; * `para-previous'
;; * `para-backward-unwrap'
;; * `para-unwrap'
;; * `para-forward-slurp'
;; * `para-forward-barf'
;; * `para-backward-slurp'
;; * `para-backward-barf'
;; * `para-transpose'
;; * `para-kill'
;; * `para-kill-hybrid'
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
;;
;; If you judge only on collection of features Smartparens may seem more
;; advanced. However, in this project I value simplicity and clarity of code
;; more than some of Smartparens' features.

;;; Code:

(require 'cl-lib)

;; TODO Write the entire thing here

(provide 'para)

;;; para.el ends here
