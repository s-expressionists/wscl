(defpackage #:wscl-evaluator
  (:use #:common-lisp)
  (:documentation "Code evaluator for WSCL")
  (:export #:parse-issue
           #:eval-issues
           #:parse-test-cases
           #:record-eval))
