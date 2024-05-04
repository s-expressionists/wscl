(in-package #:wscl-evaluator)

(defclass section ()
  ((title :accessor section-title
          :initarg :title
          :initform "")
   (text :accessor section-text
         :initarg :text
         :initform "")))

(defclass code-block ()
  ((text :accessor code-block-text
         :initarg :text
         :initform "")
   (forms :accessor code-block-forms
          :initarg :forms
          :initform nil)))

(defclass eval-form ()
  ((form :reader form
         :initarg :form)
   (record :accessor recordp
           :initform nil
           :type boolean)))

(defclass test-cases-client (eclector.parse-result:parse-result-client)
  ((last-form :accessor last-form
              :initform nil)
   (text :reader text
         :initarg :text)))

(defmethod eclector.parse-result:make-expression-result
    ((client test-cases-client) result children source)
  (declare (ignore children source))
  result)

(defmethod eclector.parse-result:make-skipped-input-result
    ((client test-cases-client) stream reason source)
  (when (and (last-form client)
             (or (eq reason :block-comment)
                 (eq (car reason) :line-comment))
             (search "=>" (text client)
                     :start2 (car source)
                     :end2 (cdr source)))
    (setf (recordp (last-form client)) t))
  nil)

(defun parse-test-cases (text)
  (with-input-from-string (stream text)
    (loop with client = (make-instance 'test-cases-client :text text)
          for form = (eclector.parse-result:read client stream nil stream)
          until (eq form stream)
          collect (setf (last-form client) (make-instance 'eval-form :form form)))))

(defun parse-issue (stream)
  (loop with text-stream = (make-string-output-stream)
        with section = nil
        for line = (read-line stream nil)
        finally (when section
                  (setf (section-text section) (get-output-stream-string text-stream)))
        while line
        unless (and (plusp (length line))
                    (char= #\: (char line (1- (length line)))))
          do (write-line line text-stream)
        else
          when section
            do (setf (section-text section) (get-output-stream-string text-stream))
          else
            collect (get-output-stream-string text-stream)
          end
          and collect (setf section (make-instance 'section :title line))
        end))

(defun eval-issue (path)
  (let ((issue (with-open-file (stream path)
                 (parse-issue stream))))
    (loop for section in issue
          when (and (typep section 'section)
                    (equalp (section-title section) "Test Cases:"))
            do (write-string (record-eval (parse-test-cases (section-text section)))))))
