;;; test-helper.el --- Helpers tests

(defmacro with-temp-buffer-str-mode (mode str &rest body)
  "Create a buffer with STR contents and MODE. "
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,str)
     (,mode)
     (goto-char (point-min))
     ,@body))
