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
;;
;; TODO: expand on usage examples

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'newcomment)


(defmacro est-deftest (name mode test-string &optional docstring)
  "Define an ERT test NAME for font-lock syntax highlighting.
TEST-STRING is the string to test, MODE is the major mode, and
DOCSTRING is a docstring to use for the test."
  (declare (indent 2) (debug t) (doc-string 4))
  `(ert-deftest ,name ()
     ,@(when docstring `(,docstring))
     (with-temp-buffer
       (insert ,test-string)
       (funcall ',mode)
       (font-lock-ensure)
       (let ((tests (est--parse-test-comments)))
         (est--check-syntax-highlighting tests)))))


(defun est--line-is-comment-p ()
  "Return t if the current line is a comment-only line."
  (save-excursion
    (beginning-of-line)
    (skip-syntax-forward " ")
    ;; skip empty lines
    (unless (eolp)
      (or
       ;; try the most convenient approach
       (looking-at "\\s<")
       ;; a bit smarter
       (and comment-start (looking-at (regexp-quote comment-start)))
       ;; hardcoded
       (and (derived-mode-p 'c-mode 'c++-mode 'java-mode)
            (looking-at-p "//"))))))

(defun est--goto-first-char ()
  "Move the point to the first character."
  (beginning-of-line)
  (skip-syntax-forward " "))

(defun est--get-first-char-column ()
  "Get the position of the first non-empty char in the current line."
  (save-excursion
    (est--goto-first-char)
    (- (point) (line-beginning-position))))

(defun est--parse-test-comments ()
  "Read test assertions from comments in the current buffer."
  (let ((tests '())
        (curline 1)
        (linetocheck -1))

    (goto-char (point-min))

    ;; Go through all lines, for comments check if there are
    ;; assertions. For non-comment line remember the last line seen.
    (while (not (eobp))
      (catch 'continue

        ;; Not a comment? remember line number
        (unless (est--line-is-comment-p)
          (setq linetocheck curline)
          (throw 'continue t))

        ;; Looking at a comment? Check if it defines assertions
        (when (re-search-forward "\\(\\^\\|<-\\) +\\(!?\\)\\([[:alnum:]\\._-]+\\)"
                                 (line-end-position) t)

          (unless (> linetocheck -1)
            (error "Trying to specify a test without a line to test"))

          ;; construct a test
          (let* (;; the line number to be checked
                 (line linetocheck)
                 ;; either comment start char column (for arrows) or
                 ;; caret column
                 (column (if (equal (match-string-no-properties 1) "^")
                             (- (match-beginning 1) (line-beginning-position))
                           (est--get-first-char-column)))
                 ;; negate the face?
                 (negation (string-equal (match-string-no-properties 2) "!"))
                 ;; the face that is supposed to be in the position specified
                 (face (match-string-no-properties 3)))

            (push (list :line line :column column :face face :negation negation) tests))))

      (cl-incf curline)
      (forward-line 1))

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

(defun est--check-syntax-highlighting (tests)
  "Check if the current buffer is fontified correctly using TESTS.

The function is meant to be run from within an ERT test."
  (dolist (test tests)
    (let* ((line (plist-get test :line))
           (column (plist-get test :column))
           (expected-face (intern (plist-get test :face)))
           (negation (plist-get test :negation))

           (actual-face (get-text-property (est--point-at-line-and-column line column) 'face))
           (line-str (est--get-line line)))

      (when (not (eq actual-face expected-face))
        (ert-fail
         (format "Expected face %s, got %s on line %d column %d: \n%s"
                 expected-face actual-face line column
                 line-str)))

      (when (and negation (eq actual-face expected-face))
        (ert-fail
         (format "Did not expect face %s face on line %d, column %d: \n%s"
                 actual-face line column
                 line-str))))))

(defun est-test-font-lock-string (test-string mode)
  "ERT test for syntax highlighting of TEST-STRING.

MODE - a major mode to use.

The function is meant to be run from within an ERT test."
  (with-temp-buffer
    (insert test-string)
    (funcall mode)
    (font-lock-ensure)

    (est--check-syntax-highlighting (est--parse-test-comments)))

  (ert-pass))

(defun est-test-font-lock-file (filename mode)
  "ERT test for syntax highlighting of FILENAME.

MODE - a major mode to use.

The function is meant to be run from within an ERT test."
  (with-temp-buffer
    (insert-file-contents filename)
    (funcall mode)
    (font-lock-ensure)

    (est--check-syntax-highlighting (est--parse-test-comments)))

  (ert-pass))


(provide 'est)

;;; est.el ends here
