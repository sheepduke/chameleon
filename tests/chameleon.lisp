(defpackage chameleon-tests.config
  (:use #:cl #:chameleon)
  (:export #:active-profile
           #:profiles
           #:server-hostname
           #:server-port))

(in-package chameleon-tests.config)

(defconfig
  (server-hostname)
  (server-port 5000))

(defprofile :dev
  (server-hostname "localhost"))

(defprofile :prod
  (server-port 8080))

(defprofile :funcall
  (server-port (lambda () 9000)))

(defpackage chameleon-tests
  (:use #:cl
        #:chameleon)
  (:import-from #:fiveam
                #:is
                #:test
                #:def-suite*))

(in-package chameleon-tests)

(def-suite* chameleon)

(test defconfig
  (is (equal nil (chameleon-tests.config:server-hostname))
      "Function server-hostname defined and return NIL")
  (is (equal 5000 (chameleon-tests.config:server-port))
      "Function server-port defined and return default value 5000"))

(test defprofile
  (is (fset:equal? (fset:set nil :dev :prod :funcall)
                   (fset:convert 'fset:set (chameleon-tests.config:profiles)))
      "Profiles contain NIL (default), :DEV, :PROD and :FUNCALL")
  (is (equal nil (chameleon-tests.config:active-profile))
      "Active profile is NIL"))

(test defprofile-dev
  (setf (chameleon-tests.config:active-profile) :dev)
  (is (string= "localhost" (chameleon-tests.config:server-hostname) )
      "Function server-hostname returns profile defined value")
  (is (= 5000 (chameleon-tests.config:server-port) )
      "Function server-port returns default value")
  (setf (chameleon-tests.config:active-profile) nil))

(test defprofile-prod
  (setf (chameleon-tests.config:active-profile) :prod)
  (is (equal nil (chameleon-tests.config:server-hostname))
      "Function server-hostname returns default value")
  (is (= 8080 (chameleon-tests.config:server-port))
      "Function server-port returns profile defined value")
  (setf (chameleon-tests.config:active-profile) :prod))

(test set-config
  ;; Setup
  (setf (chameleon-tests.config:active-profile) :dev)
  (setf (chameleon-tests.config:server-port) 8000)
  ;; Test
  (is (equal :dev (chameleon-tests.config:active-profile))
      "Active profile is properly set")
  (is (= 8000 (chameleon-tests.config:server-port))
      "Function server-port returns newly defined value")
  (setf (chameleon-tests.config:active-profile) :prod)
  (is (= 8080 (chameleon-tests.config:server-port))
      "Function server-port returns correct value for another profile")
  ;; Tear down
  (setf (chameleon-tests.config:active-profile) :dev)
  (setf (chameleon-tests.config:server-port) 5000)
  (setf (chameleon-tests.config:active-profile) nil))

(test config-with-lambda
  (setf (chameleon-tests.config:active-profile) :funcall)
  (is (= 9000 (chameleon-tests.config:server-port))
      "Function is called when the value is a function")
  (setf (chameleon-tests.config:active-profile) nil))