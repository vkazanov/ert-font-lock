;;; Code:

(require 'ert)
(require 'ert-x)

(require 'est)

(ert-deftest test-syntax-highlight--caret-multiple-faces ()
  (let ((str "
var abc = function(d) {
//   ^ font-lock-variable-name-face
    //        ^ font-lock-keyword-face
    //             ^ font-lock-variable-name-face
};

"))
    (with-temp-buffer
      (insert str)
      (javascript-mode)
      (font-lock-ensure)

      (est--check-syntax-highlighting
       (est--parse-test-comments)))))

(ert-deftest test-syntax-highlight--caret-wrong-face ()
  (let* ((str "
var abc = function(d) {
//   ^ not-a-face
};
"))
    (with-temp-buffer
      (insert str)
      (javascript-mode)
      (font-lock-ensure)

      (should-error (est--check-syntax-highlighting (est--parse-test-comments))))))

(ert-deftest test-syntax-highlight--correct ()
  (est-test-font-lock-file (ert-resource-file "correct.js") 'javascript-mode))

(ert-deftest test-syntax-highlight--wrong ()
  (should-error (est-test-font-lock-file (ert-resource-file "broken.js")  'javascript-mode)))
