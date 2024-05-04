(asdf:defsystem "wscl-evaluator"
  :description "Code evaluator for WSCL"
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on ("closer-mop"
               "eclector")
  :serial t
  :components ((:file "packages")
               (:file "eval")
               (:file "issue")))
