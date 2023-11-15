;;; Code:

(require 'ert)

(require 'est)

(ert-deftest test-parse-comments-caret-single-line-single-caret ()
  (let* ((str "
first
// ^ face.face1
")
         (asserts (est--parse-test-comments str)))
    (should (eql (length asserts) 1))
    (should (equal (car asserts)
                   '(:line 2 :column 3 :face "face.face1" :negation nil)))))

(ert-deftest test-parse-comments-caret-single-line-multiple-carets ()
  (let* ((str "
first
// ^ face1
//     ^ face.face2
//     ^ face-face.face3
   //  ^ face_face.face4
")
         (asserts (est--parse-test-comments str)))
    (should (eql (length asserts) 4))
    (should (equal asserts
                   '((:line 2 :column 3 :face "face1" :negation nil)
                     (:line 2 :column 7 :face "face.face2" :negation nil)
                     (:line 2 :column 7 :face "face-face.face3" :negation nil)
                     (:line 2 :column 7 :face "face_face.face4" :negation nil))))))
