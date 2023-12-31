;;; Code:

(require 'ert)

(require 'ert-font-lock)

(ert-deftest test-line-comment-p--fundamental ()

  (with-temp-buffer-str-mode fundamental-mode
    "// comment\n"
    (should-not (ert-font-lock--line-comment-p))))

(ert-deftest test-line-comment-p--emacs-lisp ()

  (with-temp-buffer-str-mode emacs-lisp-mode
    "not comment
;; comment
"
    (goto-line 1)
    (should-not (ert-font-lock--line-comment-p))
    (goto-line 2)
    (should (ert-font-lock--line-comment-p))
    (goto-line 3)
    (should-not (ert-font-lock--line-comment-p))))

(ert-deftest test-line-comment-p--shell-script ()

  (with-temp-buffer-str-mode shell-script-mode
    "echo Not a comment
# comment
"
    (goto-line 1)
    (should-not (ert-font-lock--line-comment-p))
    (goto-line 2)
    (should (ert-font-lock--line-comment-p))))

(ert-deftest test-line-comment-p--php ()
  (skip-unless (featurep 'php-mode))

  (with-temp-buffer-str-mode php-mode
    "echo 'Not a comment'
// comment
/* comment */
"
    (goto-line 1)
    (should-not (ert-font-lock--line-comment-p))
    (goto-line 2)
    (should (ert-font-lock--line-comment-p))
    (goto-line 3)
    (should (ert-font-lock--line-comment-p))))


(ert-deftest test-line-comment-p--javascript ()
  (with-temp-buffer-str-mode javascript-mode
    "// comment

   // comment, after a blank line

var abc = function(d) {};
"
    (goto-line 1)
    (should (ert-font-lock--line-comment-p))

    (goto-line 2)
    (should-not (ert-font-lock--line-comment-p))

    (goto-line 3)
    (should (ert-font-lock--line-comment-p))

    (goto-line 4)
    (should-not (ert-font-lock--line-comment-p))

    (goto-line 5)
    (should-not (ert-font-lock--line-comment-p))))

(ert-deftest test-line-comment-p--python ()

  (with-temp-buffer-str-mode python-mode
    "# comment

   # comment
print(\"Hello, world!\")"
    (goto-line 1)
    (should (ert-font-lock--line-comment-p))

    (goto-line 2)
    (should-not (ert-font-lock--line-comment-p))

    (goto-line 3)
    (should (ert-font-lock--line-comment-p))

    (goto-line 4)
    (should-not (ert-font-lock--line-comment-p))))

(ert-deftest test-line-comment-p--c ()

  (with-temp-buffer-str-mode c-mode
    "// comment
/* also comment */"
    (goto-line 1)
    (should (ert-font-lock--line-comment-p))

    (goto-line 2)
    (should (ert-font-lock--line-comment-p))))

(ert-deftest test-parse-comments--single-line-error ()
  (let* ((str "// ^ face.face1")
         asserts)
    (with-temp-buffer
      (insert str)
      (javascript-mode)

      (should-error (ert-font-lock--parse-comments)))))

(ert-deftest test-parse-comments--single-line-single-caret ()
  (let* ((str "
first
// ^ face.face1
")
         asserts)
    (with-temp-buffer
      (insert str)
      (javascript-mode)

      (setq asserts (ert-font-lock--parse-comments))
      (should (eql (length asserts) 1))
      (should (equal (car asserts)
                     '(:line-checked 2 :line-assert 3 :column-checked 3 :face "face.face1" :negation nil))))))

(ert-deftest test-parse-comments--caret-negation ()
  (let* ((str "
first
// ^ !face
// ^ face
")
         asserts)
    (with-temp-buffer
      (insert str)
      (javascript-mode)

      (setq asserts (ert-font-lock--parse-comments))
      (should (eql (length asserts) 2))
      (should (equal asserts
                     '((:line-checked 2 :line-assert 3 :column-checked 3 :face "face" :negation t)
                       (:line-checked 2 :line-assert 4 :column-checked 3 :face "face" :negation nil)))))))


(ert-deftest test-parse-comments--single-line-multiple-carets ()
  (let* ((str "
first
// ^ face1
//     ^ face.face2
//     ^ face-face.face3
   //  ^ face_face.face4
")
         asserts)

    (with-temp-buffer
      (insert str)
      (javascript-mode)

      (setq asserts (ert-font-lock--parse-comments))
      (should (eql (length asserts) 4))
      (should (equal asserts
                     '((:line-checked 2 :line-assert 3 :column-checked 3 :face "face1" :negation nil)
                       (:line-checked 2 :line-assert 4 :column-checked 7 :face "face.face2" :negation nil)
                       (:line-checked 2 :line-assert 5 :column-checked 7 :face "face-face.face3" :negation nil)
                       (:line-checked 2 :line-assert 6 :column-checked 7 :face "face_face.face4" :negation nil)))))))

(ert-deftest test-parse-comments--multiple-line-multiple-carets ()
  (let* ((str "
first
// ^ face1
second
// ^ face2
//   ^ face3
third
")
         asserts)
    (with-temp-buffer
      (insert str)
      (javascript-mode)

      (setq asserts (ert-font-lock--parse-comments))
      (should (eql (length asserts) 3))
      (should (equal asserts
                     '((:line-checked 2  :line-assert 3 :column-checked 3 :face "face1" :negation nil)
                       (:line-checked 4  :line-assert 5 :column-checked 3 :face "face2" :negation nil)
                       (:line-checked 4  :line-assert 6 :column-checked 5 :face "face3" :negation nil)))))))


(ert-deftest test-parse-comments--arrow-single-line-single ()
  (let* ((str "
first
// <- face1
")
         asserts)
    (with-temp-buffer
      (insert str)
      (javascript-mode)

      (setq asserts (ert-font-lock--parse-comments))
      (should (eql (length asserts) 1))
      (should (equal (car asserts)
                     '(:line-checked 2 :line-assert 3 :column-checked 0 :face "face1" :negation nil))))))


(ert-deftest test-parse-comments-arrow-multiple-line-single ()
  (let* ((str "
first
// <- face1
  // <- face2
    // <- face3
")
         asserts)
    (with-temp-buffer
      (insert str)
      (javascript-mode)

      (setq asserts (ert-font-lock--parse-comments))
      (should (eql (length asserts) 3))
      (should (equal asserts
                     '((:line-checked 2 :line-assert 3 :column-checked 0 :face "face1" :negation nil)
                       (:line-checked 2 :line-assert 4 :column-checked 2 :face "face2" :negation nil)
                       (:line-checked 2 :line-assert 5 :column-checked 4 :face "face3" :negation nil)))))))

(ert-deftest test-parse-comments--non-assert-comment-single ()
  (let* ((str "
// first
//  ^ comment-face
")
         asserts)
    (with-temp-buffer
      (insert str)
      (javascript-mode)

      (setq asserts (ert-font-lock--parse-comments))
      (should (eql (length asserts) 1))
      (should (equal (car asserts)
                     '(:line-checked 2 :line-assert 3 :column-checked 4 :face "comment-face" :negation nil))))))

(ert-deftest test-parse-comments--non-assert-comment-multiple ()
  (let* ((str "
// first second third
//  ^ comment-face
//        ^ comment-face
//                ^ comment-face
")
         asserts)
    (with-temp-buffer
      (insert str)
      (javascript-mode)

      (setq asserts (ert-font-lock--parse-comments))
      (should (eql (length asserts) 3))
      (should (equal asserts
                     '((:line-checked 2 :line-assert 3 :column-checked 4 :face "comment-face" :negation nil)
                       (:line-checked 2 :line-assert 4 :column-checked 10 :face "comment-face" :negation nil)
                       (:line-checked 2 :line-assert 5 :column-checked 18 :face "comment-face" :negation nil)))))))


(ert-deftest test-parse-comments--multiline-comment-single ()
  (let* ((str "
/*
  this is a comment
   ^ comment-face
 */
")
         asserts)
    (with-temp-buffer
      (insert str)
      (c-mode)

      (setq asserts (ert-font-lock--parse-comments))
      (should (eql (length asserts) 1))
      (should (equal (car asserts)
                     '(:line-checked 3 :line-assert 4 :column-checked 3 :face "comment-face" :negation nil))))))

(ert-deftest test-parse-comments--multiline-comment-multiple ()
  (let* ((str "
/*
  this is a comment
   ^ comment-face
  another comment
    ^ comment-face
 */
")
         asserts)
    (with-temp-buffer
      (insert str)
      (c-mode)

      (setq asserts (ert-font-lock--parse-comments))
      (should (eql (length asserts) 2))
      (should (equal asserts
                     '((:line-checked 3 :line-assert 4 :column-checked 3 :face "comment-face" :negation nil)
                       (:line-checked 5 :line-assert 6 :column-checked 4 :face "comment-face" :negation nil)))))))
