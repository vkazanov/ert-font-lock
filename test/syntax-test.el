;;; Code:

(require 'ert)

(require 'est)

(ert-deftest test-syntax-highlight-caret-simple ()
  (let ((str "
var abc = function(d) {
//   ^ font-lock-variable-name-face
    //        ^ font-lock-keyword-face
    //             ^ font-lock-variable-name-face
};

"))
    (est--check-syntax-highlighting str (est--parse-test-comments str))))

(ert-deftest test-syntax-highlight-caret-wrong-face ()
  (let* ((str "
var abc = function(d) {
//   ^ not-a-face
};
")
         (tests (est--parse-test-comments str)))
    (should-error (est--check-syntax-highlighting str tests))))
