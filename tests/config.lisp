(defpackage chameleon-tests.config
  (:use #:cl #:chameleon)
  (:export ;; #:*profile*
           #:server-hostname
           #:server-port
           #:with-profile
           #:switch-profile))

(in-package chameleon-tests.config)

(defconfig
  (server-hostname)
  (server-port 5000))

(defprofile :default)

(defprofile :dev
  (server-hostname "localhost"))

(defprofile :prod
  (server-port 8080))

(defprofile :funcall
  (server-port (lambda () 9000)))
