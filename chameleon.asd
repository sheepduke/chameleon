(defsystem chameleon
  :description "Configuration management facilities for Common Lisp."
  :author "YUE Daian"
  :license "GNU GPL v3"
  :version "0.0.1"
  :depends-on (:alexandria
               :trivia)
  :components ((:module "src"
                :components
                ((:file "packages")
                 (:file "chameleon"))))
  :in-order-to ((test-op (test-op "cl-chameleon/tests"))))

(defsystem chameleon/tests
  :description "Tests of chameleon system"
  :author "YUE Daian"
  :license "GNU GPL v3"
  :version "0.0.1"
  :depends-on (:chameleon :rove)
  :components ((:module "src"
                :components
                ((:file "packages"))))
  :perform (test-op (op c) (symbol-call :rove :run c)))
