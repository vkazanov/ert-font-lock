;;; est.el --- Emacs Lisp Syntax Testing   -*- lexical-binding: t -*-

;;; Commentary:
;;
;; TODO: more languages for test comment parsing
;; TODO: loading from example files (that have their mode specified)
;; TODO: testing convenience macro for string

;;; Code:

(require 'ert)
(require 'cl-lib)

(defun est--parse-test-comments (test-string major-mode-function)
  "Parse test comments in TEST-STRING for expected fontification in
a major mode inited by MAJOR-MODE-FUNCTION."
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
  "Check if TEST-STRING is fontified as per TESTS in a mode inited
by MAJOR-MODE-FUNCTION."
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
  "ERT test for syntax highlighting of TEST-STRING."
  (est--check-syntax-highlighting
   test-string (est--parse-test-comments test-string major-mode-function)
   major-mode-function)
  (ert-pass))


(provide 'est)

;;; est.el ends here
