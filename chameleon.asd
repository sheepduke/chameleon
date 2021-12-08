(defsystem chameleon
  :version "1.0.1"
  :description "Configuration management facilities for Common Lisp with multiple profile support."
  :license "MIT"
  :author "YUE Daian"
  :depends-on (#:alexandria)
  :serial t
  :components ((:module "src"
                :components
                ((:file "chameleon"))))
  :in-order-to ((test-op (test-op :chameleon/tests))))

(defsystem chameleon/tests
  :version "1.0.1"
  :license "MIT"
  :author "YUE Daian"
  :depends-on (#:chameleon
               #:fiveam
               #:fset)
  :serial t
  :components ((:module "tests"
                :components
                ((:file "chameleon"))))
  :perform (test-op (op c)
                    (symbol-call :fiveam :run!
                                 (find-symbol* :chameleon :chameleon-tests))))
