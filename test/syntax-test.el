;;; Code:

(require 'ert)
(require 'ert-x)

(require 'ert-font-lock)

(ert-deftest test-syntax-highlight-inline--caret-multiple-faces ()
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

      (ert-font-lock--check-faces
       (ert-font-lock--parse-comments)))))

(ert-deftest test-syntax-highlight-inline--caret-wrong-face ()
  (let* ((str "
var abc = function(d) {
//   ^ not-a-face
};
"))
    (with-temp-buffer
      (insert str)
      (javascript-mode)
      (font-lock-ensure)

      (should-error (ert-font-lock--check-faces
                     (ert-font-lock--parse-comments))))))

(ert-deftest test-syntax-highlight-file--correct ()
  (ert-font-lock-test-file
   (ert-resource-file "correct.js")
   'javascript-mode))

(ert-deftest test-syntax-highlight-file--invalid-mode ()
  (should-error (ert-font-lock-test-file
                 (ert-resource-file "correct.js")
                 'non-existing-mode)))

(ert-deftest test-syntax-highlight-file--wrong ()
  (should-error (ert-font-lock-test-file
                 (ert-resource-file "broken.js")
                 'javascript-mode)))
