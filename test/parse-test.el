;;; Code:

(require 'ert)

(require 'est)

(ert-deftest test-line-is-comment-p--fundamental ()

  (with-temp-buffer-str-mode fundamental-mode
    "// comment\n"
    (should-not (est--line-is-comment-p))))

(ert-deftest test-line-is-comment-p--emacs-lisp ()

  (with-temp-buffer-str-mode emacs-lisp-mode
    "not comment
;; comment
"
    (goto-line 1)
    (should-not (est--line-is-comment-p))
    (goto-line 2)
    (should (est--line-is-comment-p))
    (goto-line 3)
    (should-not (est--line-is-comment-p))))

(ert-deftest test-line-is-comment-p--javascript ()
  (with-temp-buffer-str-mode javascript-mode
    "// comment

   // comment, after a blank line

var abc = function(d) {};
"
    (goto-line 1)
    (should (est--line-is-comment-p))

    (goto-line 2)
    (should-not (est--line-is-comment-p))

    (goto-line 3)
    (should (est--line-is-comment-p))

    (goto-line 4)
    (should-not (est--line-is-comment-p))

    (goto-line 5)
    (should-not (est--line-is-comment-p))))

(ert-deftest test-line-is-comment-p--python ()

  (with-temp-buffer-str-mode python-mode
    "# comment

   # comment
print(\"Hello, world!\")"
    (goto-line 1)
    (should (est--line-is-comment-p))

    (goto-line 2)
    (should-not (est--line-is-comment-p))

    (goto-line 3)
    (should (est--line-is-comment-p))

    (goto-line 4)
    (should-not (est--line-is-comment-p))))

(ert-deftest test-line-is-comment-p--c ()

  (with-temp-buffer-str-mode c-mode
    "// comment
/* also comment */"
    (goto-line 1)
    (should (est--line-is-comment-p))

    (goto-line 2)
    (should (est--line-is-comment-p))))

(ert-deftest test-parse-comments--single-line-single-caret ()
  (let* ((str "
first
// ^ face.face1
")
         asserts)
    (with-temp-buffer
      (insert str)
      (javascript-mode)

      (setq asserts (est--parse-test-comments))
      (should (eql (length asserts) 1))
      (should (equal (car asserts)
                     '(:line-checked 2 :column-checked 3 :face "face.face1" :negation nil))))))

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

      (setq asserts (est--parse-test-comments))
      (should (eql (length asserts) 2))
      (should (equal asserts
                     '((:line-checked 2 :column-checked 3 :face "face" :negation t)
                       (:line-checked 2 :column-checked 3 :face "face" :negation nil)))))))


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

      (setq asserts (est--parse-test-comments))
      (should (eql (length asserts) 4))
      (should (equal asserts
                     '((:line-checked 2 :column-checked 3 :face "face1" :negation nil)
                       (:line-checked 2 :column-checked 7 :face "face.face2" :negation nil)
                       (:line-checked 2 :column-checked 7 :face "face-face.face3" :negation nil)
                       (:line-checked 2 :column-checked 7 :face "face_face.face4" :negation nil)))))))

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

      (setq asserts (est--parse-test-comments))
      (should (eql (length asserts) 3))
      (should (equal asserts
                     '((:line-checked 2 :column-checked 3 :face "face1" :negation nil)
                       (:line-checked 4 :column-checked 3 :face "face2" :negation nil)
                       (:line-checked 4 :column-checked 5 :face "face3" :negation nil)))))))


(ert-deftest test-parse-comments--arrow-single-line-single ()
  (let* ((str "
first
// <- face1
")
         asserts)
    (with-temp-buffer
      (insert str)
      (javascript-mode)

      (setq asserts (est--parse-test-comments))
      (should (eql (length asserts) 1))
      (should (equal (car asserts)
                     '(:line-checked 2 :column-checked 0 :face "face1" :negation nil))))))


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

      (setq asserts (est--parse-test-comments))
      (should (eql (length asserts) 3))
      (should (equal asserts
                     '((:line-checked 2 :column-checked 0 :face "face1" :negation nil)
                       (:line-checked 2 :column-checked 2 :face "face2" :negation nil)
                       (:line-checked 2 :column-checked 4 :face "face3" :negation nil)))))))
