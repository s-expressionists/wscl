(in-package #:wscl-evaluator)

(defclass current-practice ()
  ((implementation :accessor implementation
                   :initarg :implementation)
   (version :accessor version
            :initarg :version)
   (test :accessor text
         :initarg :text
         :initform "")))

(defun make-current-practice (x)
           (let ((pos (position #\space x :start 2)))
             (make-instance 'current-practice
                            :implementation (subseq x 2 pos)
                            :version (subseq x (1+ pos)))))

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

(defmethod eclector.reader:interpret-symbol
    ((client test-cases-client) input-stream
     package-indicator symbol-name internp)
  (prog (package)
   set-package
     (setf package (case package-indicator
                     (:current (eclector.reader:state-value client '*package*))
                     (:keyword (find-package "KEYWORD"))
                     (t        (or (find-package package-indicator)
                                   (multiple-value-bind (value kind)
                                       (eclector.reader:package-does-not-exist
                                        input-stream
                                        package-indicator symbol-name internp)
                                     (ecase kind
                                       (symbol
                                        (return value))
                                       (package
                                        value)
                                       (:package-name
                                        (setf package-indicator value)
                                        (go set-package))))))))
     (return (if internp
               (intern symbol-name package)
               (multiple-value-bind (symbol status)
                   (find-symbol symbol-name package)
                 (cond ((null status)
                        (eclector.reader:symbol-does-not-exist
                         input-stream package symbol-name))
                       ((eq status :internal)
                        (eclector.reader:symbol-is-not-external
                         input-stream package symbol-name symbol))
                       (t
                        symbol)))))))

(defun parse-test-cases (text)
  (let ((forms (with-input-from-string (stream text)
                 (loop with client = (make-instance 'test-cases-client :text text)
                       for form = (eclector.parse-result:read client stream nil stream)
                       until (eq form stream)
                       collect (setf (last-form client) (make-instance 'eval-form :form form))))))
    (if (some #'recordp forms)
        forms
        nil)))

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

(defun current-practice< (x y)
  (or (string< (implementation x)
               (implementation y))
      (uiop:version< (version x)
                     (version y))))

(defun same-practice-p (x y)
  (and x y
       (equalp (implementation x) (implementation y))
       (equalp (text y) (text y))))

(defun same-implementation-p (x y)
  (and x y
       (equalp (implementation x) (implementation y))))

(defun same-version-p (x y)
  (and x y
       (equalp (implementation x) (implementation y))
       (equalp (version x) (version y))))

(defun remove-practice-p (previous current next)
  (or (same-version-p previous current)
      (same-version-p current next)
      (and (not (same-implementation-p previous current))
           (same-practice-p current next))
      (and (not (same-implementation-p current next))
           (same-practice-p previous current))
      (and (same-practice-p previous current)
           (same-practice-p next current))))

(defun write-current-practice (stream text)
  (let ((items (loop with stream = (make-string-input-stream text)
                     with current = nil
                     for line = (read-line stream nil)
                     while line
                     when (and (> (length line) 2)
                               (char/= (char line 2) #\Space))
                       collect (setf current (make-current-practice line))
                     else when (and (plusp (length line))
                                    current)
                            do (setf (text current)
                                     (concatenate 'string
                                                  (text current)
                                                  '(#\newline)
                                                  line)))))
    (setf items (sort items #'current-practice<))
    (loop with previous = nil
          for (current next) on items
          unless (remove-practice-p previous current next)
            do (setf previous current)
               (format stream "  ~a ~a~a~%~%"
                       (implementation current)
                       (version current)
                       (text current)))))

(defun eval-issue (path)
  (format t "~%Parsing and evaluating ~a...~%" path)
  (handler-case
      (let* ((*package* (make-package (gensym) :use '(:common-lisp)))
             (update nil)
             (issue (with-open-file (stream path)
                      (parse-issue stream)))
             (text (with-output-to-string (stream)
                     (loop with last-current-practice = nil
                           for section in issue
                           when (stringp section)
                             do (write-string section stream)
                           else when (and last-current-practice
                                          (equalp (section-title section) "Current Practice:"))
                                  do (write-line (section-title section) stream)
                                     (terpri stream)
                                     (write-current-practice stream
                                                             (concatenate 'string
                                                                          (section-text section)
                                                                          last-current-practice))
                                     (setf last-current-practice nil)
                           else
                             do (write-line (section-title section) stream)
                                (write-string (section-text section) stream)
                           when (and (typep section 'section)
                                     (equalp (section-title section) "Test Cases:"))
                             do (let ((forms (parse-test-cases (section-text section))))
                                  (when forms
                                    (setf update t
                                          last-current-practice (record-eval forms))))))))
        (cond (update
               (with-open-file (stream path :direction :output :if-exists :supersede)
                 (write-string text stream))
               (write-line "Issue updated."))
              (otherwise
               (write-line "Issue skipped due to no valid test cases."))))
    (error (condition)
      (format *error-output* "Unable to update ~a because of error:~%  ~a~&"
              path condition))))

(defun eval-issues (&optional (path #P"wscl-issues/draft/*"))
  (loop for path in (sort (directory path) #'string-lessp :key #'namestring)
        when (uiop:file-pathname-p path)
          do (eval-issue path)))
