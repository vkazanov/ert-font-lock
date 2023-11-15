;;; Code:

(require 'ert)

(require 'est)

(ert-deftest test-syntax-highlight-caret-multiple-faces ()
  (let ((str "
var abc = function(d) {
//   ^ font-lock-variable-name-face
    //        ^ font-lock-keyword-face
    //             ^ font-lock-variable-name-face
};

"))
    (est--check-syntax-highlighting
     str
     (est--parse-test-comments str 'javascript-mode)
     'javascript-mode)))

(ert-deftest test-syntax-highlight-caret-wrong-face ()
  (let* ((str "
var abc = function(d) {
//   ^ not-a-face
};
")
         (tests (est--parse-test-comments str 'javascript-mode)))
    (should-error (est--check-syntax-highlighting str tests 'javascript-mode))))

(ert-deftest test-syntax-highlight-arrow-face ()
  (est-test-font-lock-string "
var abc = function(d) {
//   <- font-lock-keyword-face
};
" 'javascript-mode))

(ert-deftest test-syntax-highlight-arrow-wrong ()
  (should-error (est-test-font-lock-string "
var abc = function(d) {
//   <- not-face
};
" 'javascript-mode)))
