(in-package #:wscl-evaluator)

(defun record-eval (forms &aux (package *package*))
  (with-standard-io-syntax
    (let ((*print-pretty* t)
          (*print-readably* nil)
          (*print-circle* t)
          (*print-right-margin* 72)
          (*package* package))
      (with-output-to-string (stream)
        (write-string "  " stream)
        (pprint-logical-block (stream forms)
          (format stream "~A ~A" (uiop:implementation-type) (uiop:lisp-version-string))
          (loop for form = (pprint-pop)
                while form
                when (recordp form)
                  do (pprint-newline :mandatory stream)
                     (write-string "  " stream)
                     (pprint-logical-block (stream nil)
                       (write (form form) :stream stream :case :downcase)
                       (write-char #\Space stream)
                       (pprint-newline :linear stream)
                       (pprint-logical-block (stream nil :per-line-prefix "; ")
                         (write-string "=> " stream)
                         (pprint-indent :current 0 stream)
                         (handler-case
                             (multiple-value-list (eval (form form)))
                           (type-error (condition)
                             (format stream "~@<[~;signals TYPE-ERROR~@[ with expected type ~S~]~;]~:@>"
                                     (ignore-errors
                                      (type-error-expected-type condition))))
                           (error (condition)
                             (format stream "~@<[~;signals ~S~;]~:@>"
                                     (or (find-if (lambda (class-name)
                                                    (and (or (equal "ERROR" (symbol-name class-name))
                                                             (equal 0 (mismatch "ERROR" (symbol-name class-name) :from-end t)))
                                                         (equal "COMMON-LISP" (package-name (symbol-package class-name)))))
                                                  (mapcar #'class-name (closer-mop:compute-class-precedence-list (class-of condition))))
                                         (class-name (class-of condition)))))
                           (:no-error (result)
                             (format stream "~{~S~^, ~_~}" result)))))
                else
                  do (eval (form form))))))))
