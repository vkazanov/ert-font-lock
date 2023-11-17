;;; Code:

(require 'ert)

(require 'est)

(est-deftest test-macro-test--correct-highlighting emacs-lisp-mode
  "
(defun fun ())
;; ^ font-lock-keyword-face
;;      ^ font-lock-function-name-face")

(est-deftest test-macro-test--docstring emacs-lisp-mode
  "
(defun fun ())
;; ^ font-lock-keyword-face"
  "An EST test with a docstring.")
