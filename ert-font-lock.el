;;; ert-font-lock.el --- ERT Font Lock   -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2023 Vladimir Kazanov

;; Maintainer: Vladimir Kazanov
;; Keywords: lisp, test
;; URL: https://github.com/vkazanov/ert-font-lock
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
;; ERT Font Lock is an extension to standard Emacs Lisp Regression
;; Test library (ERT) providing a convenient way to check syntax
;; highlighting provided by font-lock.
;;
;; ert-font-lock entry points are functions
;; `ert-font-lock-test-string' and `ert-font-lock-test-file' and
;; covenience test-defining macros: `ert-font-lock-deftest' and
;; `ert-font-lock-deftest-file'.
;;
;; See ert-font-lock ERT unit tests for
;; usage examples.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'newcomment)


(defun ert-font-lock--validate-major-mode (mode)
  "Validate if MODE is a valid major mode."
  (unless (functionp mode)
    (error "Invalid major mode: %s. Please specify a valid major mode for
syntax highlighting tests" mode)))


(defmacro ert-font-lock-deftest (name mode test-string &optional docstring)
  "Define an ERT test NAME for font-lock syntax highlighting.
TEST-STRING is the string to test, MODE is the major mode, and
DOCSTRING is a docstring to use for the test."
  (declare (indent 2) (debug t) (doc-string 4))
  `(ert-deftest ,name ()
     ,@(when docstring `(,docstring))
     (ert-font-lock--validate-major-mode ',mode)
     (with-temp-buffer
       (insert ,test-string)
       (funcall ',mode)
       (font-lock-ensure)
       (let ((tests (ert-font-lock--parse-comments)))
         (ert-font-lock--check-faces tests)))))


(defmacro ert-font-lock-deftest-file (name mode file &optional docstring)
  "Define an ERT test NAME for font-lock syntax highlighting.
FILE is the path to a file in ert resource dir with test cases,
MODE is the major mode, and DOCSTRING is a docstring to use for
the test."
  (declare (indent 2) (debug t) (doc-string 4))
  `(ert-deftest ,name ()
     ,@(when docstring `(,docstring))
     (ert-font-lock--validate-major-mode ',mode)
     (ert-font-lock-test-file (ert-resource-file ,file) ',mode)))


(defun ert-font-lock--line-comment-p ()
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
       (and comment-start-skip (looking-at comment-start-skip))
       ;; hardcoded
       (and (derived-mode-p 'c-mode 'c++-mode 'java-mode)
            (looking-at-p "//"))))))

(defun ert-font-lock--goto-first-char ()
  "Move the point to the first character."
  (beginning-of-line)
  (skip-syntax-forward " "))

(defun ert-font-lock--get-first-char-column ()
  "Get the position of the first non-empty char in the current line."
  (save-excursion
    (ert-font-lock--goto-first-char)
    (- (point) (line-beginning-position))))

(defun ert-font-lock--parse-comments ()
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
        (unless (ert-font-lock--line-comment-p)
          (setq linetocheck curline)
          (throw 'continue t))

        ;; Looking at a comment? Check if it defines assertions
        (when (re-search-forward "\\(\\^\\|<-\\) +\\(!?\\)\\([[:alnum:]\\._-]+\\)"
                                 (line-end-position) t)

          (unless (> linetocheck -1)
            (error "Invalid test comment syntax at line %d. Expected a line to test before the comment line" curline))

          ;; construct a test
          (let* (;; either comment start char column (for arrows) or
                 ;; caret column
                 (column-checked (if (equal (match-string-no-properties 1) "^")
                                     (- (match-beginning 1) (line-beginning-position))
                                   (ert-font-lock--get-first-char-column)))
                 ;; negate the face?
                 (negation (string-equal (match-string-no-properties 2) "!"))
                 ;; the face that is supposed to be in the position specified
                 (face (match-string-no-properties 3)))

            (push (list :line-checked linetocheck
                        :line-assert curline
                        :column-checked column-checked
                        :face face
                        :negation negation)
                  tests))))

      (cl-incf curline)
      (forward-line 1))

    (reverse tests)))

(defun ert-font-lock--point-at-line-and-column (line column)
  "Get the buffer position for LINE and COLUMN."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column column)
    (point)))

(defun ert-font-lock--get-line (line-number)
  "Return the content of the line specified by LINE-NUMBER."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line-number))
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun ert-font-lock--check-faces (tests)
  "Check if the current buffer is fontified correctly.
TESTS - tests to run.

The function is meant to be run from within an ERT test."
  (dolist (test tests)
    (let* ((line-checked (plist-get test :line-checked))
           (line-assert (plist-get test :line-assert))
           (column-checked (plist-get test :column-checked))
           (expected-face (intern (plist-get test :face)))
           (negation (plist-get test :negation))

           (actual-face (get-text-property (ert-font-lock--point-at-line-and-column line-checked column-checked) 'face))
           (line-str (ert-font-lock--get-line line-checked))
           (line-assert-str (ert-font-lock--get-line line-assert)))

      (when (not (eq actual-face expected-face))
        (ert-fail
         (list (format "Expected face %s, got %s on line %d column %d"
                       expected-face actual-face line-checked column-checked)
               :line line-str
               :assert line-assert-str)))

      (when (and negation (eq actual-face expected-face))
        (ert-fail
         (list (format "Did not expect face %s face on line %d, column %d"
                       actual-face line-checked column-checked)
               :line line-str
               :assert line-assert-str))))))

(defun ert-font-lock-test-string (test-string mode)
  "Check font faces in TEST-STRING set by MODE.

The function is meant to be run from within an ERT test."
  (ert-font-lock--validate-major-mode mode)
  (with-temp-buffer
    (insert test-string)
    (funcall mode)
    (font-lock-ensure)

    (ert-font-lock--check-faces (ert-font-lock--parse-comments)))

  (ert-pass))

(defun ert-font-lock-test-file (filename mode)
  "Check font faces in FILENAME set by MODE.

The function is meant to be run from within an ERT test."
  (ert-font-lock--validate-major-mode mode)
  (with-temp-buffer
    (insert-file-contents filename)
    (funcall mode)
    (font-lock-ensure)

    (ert-font-lock--check-faces (ert-font-lock--parse-comments)))

  (ert-pass))


(provide 'ert-font-lock)

;;; ert-font-lock.el ends here