;;; est.el --- Emacs Lisp Syntax Testing   -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'ert)
(require 'cl-lib)

(defun est--parse-test-comments (test-string)
  "Parse test comments in TEST-STRING for expected fontification."
  (let ((tests '())
        ;; start with the second line
        (curline 2)
        (linetocheck -1))
    (with-temp-buffer
      (insert test-string)

      ;; TODO: got to be mode-agnostic
      (javascript-mode)

      ;; skip the first line
      (goto-char (point-min))
      (forward-line)

      ;; look for assertions
      (while (not (eobp))
        (message "Line: %s" (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

        (skip-syntax-forward " ")
        ;; looking at at comment start or a proper line to be highlighted?
        (if (or (looking-at "\\s<")
                (looking-at comment-start))
            (progn
              ;; looks like a comment

              ;; reset to the beginning of the comment body
              (comment-beginning)

              ;; start looking for carets
              (while (re-search-forward "\\(\\^\\) +\\(!?\\)\\([[:alnum:]\\._-]+\\)" (line-end-position) t)
                ;; TODO: this should just be a warning
                (cl-assert (> linetocheck -1))

                ;; TODO: check if the face exists, report if it
                ;; doesn't and do not register the test in this case

                ;; TODO: negation: just add to the regexp and check for it

                (let* (;; the line to be checked
                       (line linetocheck)
                       ;; line start - match-beg = column
                       (column (- (match-beginning 1) (line-beginning-position)))
                       ;; negate the face?
                       (negation (string-equal (match-string 2) "!"))
                       ;; the face that is supposed to be in the position specified
                       (face (match-string 3))

                       ;; the test itself
                       (test (list :line line :column column :face face :negation negation)) )

                  (message "Found a caret: %s" test)
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

(defun est--check-syntax-highlighting (test-string tests)
  "Check if TEST-STRING is fontified as per TESTS."
  (with-temp-buffer
    (insert test-string)

    ;; fontify
    (javascript-mode)
    (font-lock-ensure)

    ;; check faces specified by TESTS
    (dolist (test tests)
      (let* ((line (plist-get test :line))
             (column (plist-get test :column))
             (expected-face (plist-get test :face))
             (negation (plist-get test :negation))
             (actual-face (get-text-property (est--point-at-line-and-column line column) 'face)))
        (if negation
            (should-not (eq actual-face (intern expected-face)))
          (should (eq actual-face (intern expected-face))))))))

(defun ert-test-syntax-highlighting (test-string)
  "ERT test for syntax highlighting of TEST-STRING."
  (let ((tests (est--parse-test-comments test-string)))
    (est--check-syntax-highlighting test-string tests)))


(provide 'est)

;;; est.el ends here
