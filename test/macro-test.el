;;; Code:

(require 'ert)

(require 'ert-font-lock)

(ert-font-lock-deftest test-macro-test--correct-highlighting emacs-lisp-mode
                       "
(defun fun ())
;; ^ font-lock-keyword-face
;;      ^ font-lock-function-name-face")

(ert-font-lock-deftest test-macro-test--docstring emacs-lisp-mode
                       "
(defun fun ())
;; ^ font-lock-keyword-face"
                       "An EST test with a docstring.")
