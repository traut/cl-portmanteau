(asdf:defsystem #:portmanteau-tests
  :depends-on (#:portmanteau #:fiveam)
  :perform (test-op (o s)
              (uiop:symbol-call :portmanteau-tests :all-tests))
  :components ((:file "portmanteau-tests")))
