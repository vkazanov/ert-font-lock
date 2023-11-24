;;; Code:

(require 'ert)
(require 'ert-font-lock)

(ert-font-lock-deftest test-macro-test--correct-highlighting
    emacs-lisp-mode
  "
(defun fun ())
;; ^ font-lock-keyword-face
;;      ^ font-lock-function-name-face")

(ert-font-lock-deftest test-macro-test--docstring
    "A test with a docstring."
  emacs-lisp-mode
  "
(defun fun ())
;; ^ font-lock-keyword-face"
  )

(ert-font-lock-deftest test-macro-test--failing
    "A failing test."
  :expected-result :failed
  emacs-lisp-mode
  "
(defun fun ())
;; ^ wrong-face")

(ert-font-lock-deftest-file test-macro-test--file
    "Test reading correct assertions from a file"
  javascript-mode
  "correct.js")

(ert-font-lock-deftest-file test-macro-test--file-failing
    "Test reading wrong assertions from a file"
  :expected-result :failed
  javascript-mode
  "broken.js")
