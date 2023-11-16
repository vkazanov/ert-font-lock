;;; est.el --- Emacs Lisp Syntax Testing   -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2023 Vladimir Kazanov

;; Author: Vladimir Kazanov
;; Keywords: lisp, test
;; URL: https://github.com/vkazanov/est
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

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

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Emacs Lisp Syntax Testing is an extension to standard Emacs Lisp
;; Regresstion Test tool providing a convenient way to check syntax
;; highlighting provided by font-lock.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'newcomment)


(defun est--line-is-comment-p ()
  "Return t if the current line is a comment-only line."
  (save-excursion
    (beginning-of-line)
    (skip-syntax-forward " ")
    (or (looking-at "\\s<")
        (looking-at comment-start))))

(defun est--goto-first-char ()
  "Move the point to the first character."
  (beginning-of-line)
  (skip-syntax-forward " "))

(defun est--get-first-char-column ()
  "Get the position of the first non-empty char in the current line."
  (save-excursion
    (est--goto-first-char)
    (- (point) (line-beginning-position))))

(defun est--parse-test-comments (test-string major-mode-function)
  "Read test assertions from comments in TEST-STRING.

MAJOR-MODE-FUNCTION - a function that would start a major mode."
  (let ((tests '())
        (curline 1)
        (linetocheck -1))

    (with-temp-buffer
      (insert test-string)
      (funcall major-mode-function)
      (goto-char (point-min))

      ;; Go through all lines, for comments check if there are
      ;; assertions. For non-comment line remember the last line seen.
      (while (not (eobp))
        (catch 'continue

          ;; Not a comment? remember line number
          (unless (est--line-is-comment-p)
            (setq linetocheck curline)
            (throw 'continue t))

          ;; A comment? Check if it defines assertions
          (when (re-search-forward "\\(\\^\\|<-\\) +\\(!?\\)\\([[:alnum:]\\._-]+\\)" (line-end-position) t)

            (unless (> linetocheck -1)
              (error "Trying to specify a test without a line to test"))

            (let* (;; the line to be checked
                   (line linetocheck)
                   ;; either comment start (for arrows) or caret
                   ;; column
                   (column (if (equal (match-string 1) "^")
                               (- (match-beginning 1) (line-beginning-position))
                             (est--get-first-char-column)))
                   ;; negate the face?
                   (negation (string-equal (match-string 2) "!"))
                   ;; the face that is supposed to be in the position specified
                   (face (match-string 3)))

              (push (list :line line :column column :face face :negation negation) tests))))

        (cl-incf curline)
        (forward-line 1)))

    (reverse tests)))

(defun est--point-at-line-and-column (line column)
  "Get the buffer position for LINE and COLUMN."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column column)
    (point)))

(defun est--get-line (line-number)
  "Return the content of the line specified by LINE-NUMBER."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line-number))
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun est--check-syntax-highlighting (test-string tests major-mode-function)
  "Check if TEST-STRING is fontified correctly.
TESTS - list of tests to run.

MAJOR-MODE-FUNCTION - a function that would start a major mode."
  (with-temp-buffer
    (insert test-string)

    ;; fontify
    (funcall major-mode-function)
    (font-lock-ensure)

    ;; check faces specified by TESTS
    (dolist (test tests)
      (let* ((line (plist-get test :line))
             (column (plist-get test :column))
             (expected-face (intern (plist-get test :face)))
             (negation (plist-get test :negation))

             (actual-face (get-text-property (est--point-at-line-and-column line column) 'face))
             (line-str (est--get-line line)))

        (when (not (eq actual-face expected-face))
          (ert-fail
           (list (format "Expected face %s, got %s on line %d column %d: \n%s"
                         actual-face expected-face line column
                         line-str))))

        (when (and negation (eq actual-face expected-face))
          (ert-fail
           (list (format "Did not expect face %s face on line %d, column %d: \n%s"
                         actual-face line column
                         line-str))))))))

(defun est-test-font-lock-string (test-string major-mode-function)
  "ERT test for syntax highlighting of TEST-STRING.

MAJOR-MODE-FUNCTION - a function that starts the major mode.

The function is meant to be run from within an ert test."
  (est--check-syntax-highlighting
   test-string (est--parse-test-comments test-string major-mode-function)
   major-mode-function)
  (ert-pass))


(provide 'est)

;;; est.el ends here
