(asdf:defsystem #:portmanteau
  :description "cl-portmanteau"
  :author "Sergey Polzunov <sergey@polzunov.com>"
  :license "OSI approved 3-clause 'New BSD License'"
  :in-order-to ((test-op (test-op #:portmanteau-tests)))
  :depends-on (#:vom)
  :serial t
  :components ((:file "portmanteau")))
