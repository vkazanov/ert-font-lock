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

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'newcomment)

(defun est--parse-test-comments (test-string major-mode-function)
  "Read test assertions from comments in TEST-STRING.

MAJOR-MODE-FUNCTION - a function that would start a major mode."
  (let ((tests '())
        ;; start with the second line
        (curline 2)
        (linetocheck -1)
        commentcol
        caretcol)
    (with-temp-buffer
      (insert test-string)

      (funcall major-mode-function)

      ;; skip the first line
      (goto-char (point-min))
      (forward-line)

      ;; look for assertions
      (while (not (eobp))
        (skip-syntax-forward " ")

        ;; looking at at comment start or a proper line to be highlighted?
        (if (or (looking-at "\\s<")
                (looking-at comment-start))
            (progn
              ;; remember the comment start column
              (setq commentcol (- (point) (line-beginning-position)))

              ;; reset to the beginning of the comment body
              (comment-beginning)

              ;; looking up the caret
              (when (re-search-forward "\\(\\^\\|<-\\) +\\(!?\\)\\([[:alnum:]\\._-]+\\)" (line-end-position) t)
                ;; remember the caret/arrow position
                (setq caretcol (- (match-beginning 1) (line-beginning-position)))

                (unless (> linetocheck -1)
                  (error "Trying to specify a test without a line to test"))

                (let* (;; the line to be checked
                       (line linetocheck)
                       ;; either comment start (for arrows) or caret
                       ;; column
                       (column (if (equal (match-string 1) "^")
                                   caretcol
                                 commentcol))
                       ;; negate the face?
                       (negation (string-equal (match-string 2) "!"))
                       ;; the face that is supposed to be in the position specified
                       (face (match-string 3))

                       ;; the test itself
                       (test (list :line line :column column :face face :negation negation)) )

                  (push test tests))))
          ;; else: not a comment, remember it
          (setq linetocheck curline))
        (forward-line)
        (cl-incf curline)))

    (reverse tests)))

(defun est--point-at-line-and-column (line column)
  "Get the buffer position for LINE and COLUMN."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column column)
    (point)))


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
             (actual-face (get-text-property (est--point-at-line-and-column line column) 'face)))

        (when (not (eq actual-face expected-face))
          (ert-fail
           (list (format "Expected face %s, got %s on line %d column %d"
                         actual-face expected-face line column))))

        (when (and negation (eq actual-face expected-face))
          (ert-fail
           (list (format "Did not expect face %s face on line %d, column %d"
                         actual-face line column))))))))

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
