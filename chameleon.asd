(defsystem chameleon
  :description "Configuration management facilities for Common Lisp with multiple profile support."
  :author "YUE Daian"
  :license "MIT"
  :version "1.0.0"
  :depends-on (:alexandria
               :trivia)
  :components ((:module "src"
                :components
                ((:file "packages")
                 (:file "chameleon"))))
  :in-order-to ((test-op (test-op "chameleon/tests"))))

(defsystem chameleon/tests
  :description "Tests of chameleon system"
  :author "YUE Daian"
  :license "GNU GPL v3"
  :version "0.0.1"
  :depends-on (:chameleon :rove)
  :components ((:module "tests"
                :components
                ((:file "packages")
                 (:file "chameleon"))))
  :perform (test-op (op c) (symbol-call :rove :run c)))

(defsystem chameleon
  :version "1.0.1"
  :description "Configuration management facilities for Common Lisp with multiple profile support."
  :license "MIT"
  :author "YUE Daian"
  :depends-on (#:alexandria
               #:trivia)
  :serial t
  :components ((:module "src"
                :components
                ((:file "packages")
                 (:file "chameleon"))))
  :in-order-to ((test-op (test-op :chameleon/tests))))

(defsystem chameleon/tests
  :version "1.0.1"
  :license "MIT"
  :author "YUE Daian"
  :depends-on (#:chameleon
               #:fiveam
               #:fset)
  :serial t
  :components ((:module "test"
                :components
                ((:file "packages")
                 (:file "chameleon"))))
  :perform (test-op (op c)
                    (symbol-call :fiveam :run!
                                 (find-symbol* :chameleon :chameleon-test))))
