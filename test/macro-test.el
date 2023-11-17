;;; Code:

(require 'ert)

(require 'est)

(est-deftest test-macro-test--correct-highlighting emacs-lisp-mode
  "
(defun fun ())
;; ^ font-lock-keyword-face
;;      ^ font-lock-function-name-face")
