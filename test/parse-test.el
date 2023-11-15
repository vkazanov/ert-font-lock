;;; Code:

(require 'ert)

(require 'est)

(ert-deftest test-parse-comments-single-line-single-caret ()
  (let* ((str "
first
// ^ face.face1
")
         (asserts (est--parse-test-comments str 'javascript-mode)))
    (should (eql (length asserts) 1))
    (should (equal (car asserts)
                   '(:line 2 :column 3 :face "face.face1" :negation nil)))))

(ert-deftest test-parse-comments-caret-negation ()
  (let* ((str "
first
// ^ !face
// ^ face
")
         (asserts (est--parse-test-comments str 'javascript-mode)))
    (should (eql (length asserts) 2))
    (should (equal asserts
                   '((:line 2 :column 3 :face "face" :negation t)
                     (:line 2 :column 3 :face "face" :negation nil))))))


(ert-deftest test-parse-comments-single-line-multiple-carets ()
  (let* ((str "
first
// ^ face1
//     ^ face.face2
//     ^ face-face.face3
   //  ^ face_face.face4
")
         (asserts (est--parse-test-comments str 'javascript-mode)))
    (should (eql (length asserts) 4))
    (should (equal asserts
                   '((:line 2 :column 3 :face "face1" :negation nil)
                     (:line 2 :column 7 :face "face.face2" :negation nil)
                     (:line 2 :column 7 :face "face-face.face3" :negation nil)
                     (:line 2 :column 7 :face "face_face.face4" :negation nil))))))

(ert-deftest test-parse-comments-multiple-line-multiple-carets ()
  (let* ((str "
first
// ^ face1
second
// ^ face2
//   ^ face3
third
")
         (asserts (est--parse-test-comments str 'javascript-mode)))
    (should (eql (length asserts) 3))
    (should (equal asserts
                   '((:line 2 :column 3 :face "face1" :negation nil)
                     (:line 4 :column 3 :face "face2" :negation nil)
                     (:line 4 :column 5 :face "face3" :negation nil))))))


(ert-deftest test-parse-comments-arrow-single-line-single ()
  (let* ((str "
first
// <- face1
")
         (asserts (est--parse-test-comments str 'javascript-mode)))
    (should (eql (length asserts) 1))
    (should (equal (car asserts)
                   '(:line 2 :column 0 :face "face1" :negation nil)))))


(ert-deftest test-parse-comments-arrow-multiple-line-single ()
  (let* ((str "
first
// <- face1
  // <- face2
    // <- face3
")
         (asserts (est--parse-test-comments str 'javascript-mode)))
    (should (eql (length asserts) 3))
    (should (equal asserts
                   '((:line 2 :column 0 :face "face1" :negation nil)
                     (:line 2 :column 2 :face "face2" :negation nil)
                     (:line 2 :column 4 :face "face3" :negation nil))))))
