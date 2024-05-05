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

(defun eval-issues (&optional (paths (uiop:command-line-arguments)))
  (loop for path in paths
        for issue = (with-open-file (stream path)
                      (parse-issue stream))
        do (with-open-file (stream "t" :direction :output :if-exists :supersede)
             (loop with last-current-practice = nil
                   for section in issue
                   when (stringp section)
                     do (write-string section stream)
                   else when (and last-current-practice
                                  (equalp (section-title section) "Current Practice:"))
                          do (write-line (section-title section) stream)
                             (let ((pos (search last-current-practice (section-text section)
                                                :end1 6)))
                               (cond (pos
                                      (write-string (section-text section) stream :end pos)
                                      (write-string last-current-practice stream)
                                      (write-string (section-text section) stream
                                                    :start (search (make-string 2 :initial-element #\Newline)
                                                                   (section-text section)
                                                                   :start2 pos)))
                                     (t
                                      (write-string (section-text section) stream)
                                      (write-string last-current-practice stream)
                                      (terpri stream)
                                      (terpri stream))))
                   else
                     do (write-line (section-title section) stream)
                        (write-string (section-text section) stream)
                   when (and (typep section 'section)
                             (equalp (section-title section) "Test Cases:"))
                     do (setf last-current-practice
                              (record-eval (parse-test-cases (section-text section))))))))
