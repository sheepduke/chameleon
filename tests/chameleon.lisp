(defpackage chameleon-tests
  (:use #:cl)
  (:local-nicknames (#:config #:chameleon-tests.config))
  (:import-from #:fiveam
                #:signals
                #:is
                #:test
                #:def-suite*))

(in-package chameleon-tests)

(def-suite* chameleon)

(test defconfig
  (let ((config:*profile* :default))
    (is (equal nil (config:server-hostname))
        "Function server-hostname defined and return NIL")
    (is (equal 5000 (config:server-port))
        "Function server-port defined and return default value 5000")))

(test defprofile-dev
  (let ((config:*profile* :dev))
    (is (string= "localhost" (config:server-hostname) )
        "Function server-hostname returns profile defined value")
    (is (= 5000 (config:server-port) )
        "Function server-port returns default value")))

(test defprofile-prod
  (let ((config:*profile* :prod))
    (is (equal nil (config:server-hostname))
        "Function server-hostname returns default value")
    (is (= 8080 (config:server-port))
        "Function server-port returns profile defined value")))

(test set-config
  ;; Setup
  (setf config:*profile* :dev)
  (setf (config:server-port) 8000)
  ;; Test
  (is (= 8000 (config:server-port))
      "Function server-port returns newly defined value")
  (setf config:*profile* :prod)
  (is (= 8080 (config:server-port))
      "Function server-port returns correct value for another profile")
  ;; Tear down
  (let ((config:*profile* :dev))
    (setf (config:server-port) 5000))
  (setf config:*profile* nil))

(test config-with-lambda
  (let ((config:*profile* :funcall))
    (is (= 9000 (config:server-port))
        "Function is called when the value is a function")))
