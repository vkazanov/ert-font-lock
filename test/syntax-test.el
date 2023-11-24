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


(ert-deftest test-syntax-highlight-inline--comment-face ()
  (let* ((str "
// this is a comment
//   ^ font-lock-comment-face
//       ^ font-lock-comment-face
//            ^ font-lock-comment-face
"))
    (with-temp-buffer
      (insert str)
      (javascript-mode)
      (font-lock-ensure)

      (ert-font-lock--check-faces
       (ert-font-lock--parse-comments)))))


(ert-deftest test-syntax-highlight-inline--multiline-comment-face ()
  (let* ((str "
/*
  this is a comment
   ^ font-lock-comment-face
  another comment
  more comments
    ^ font-lock-comment-face
 */
"))
    (with-temp-buffer
      (insert str)
      (c-mode)
      (font-lock-ensure)

      (ert-font-lock--check-faces
       (ert-font-lock--parse-comments)))))


(ert-deftest test-font-lock-test-string--correct ()
  (ert-font-lock-test-string
   "
var abc = function(d) {
// <- font-lock-keyword-face
//   ^ font-lock-variable-name-face
    //        ^ font-lock-keyword-face
    //             ^ font-lock-variable-name-face
};

"
   'javascript-mode))

(ert-deftest test-font-lock-test-file--correct ()
  (ert-font-lock-test-file
   (ert-resource-file "correct.js")
   'javascript-mode))

(ert-deftest test-font-lock-test-file--wrong ()
  :expected-result :failed
  (ert-font-lock-test-file
   (ert-resource-file "broken.js")
   'javascript-mode))
