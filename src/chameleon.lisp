(defpackage chameleon
  (:use #:cl)
  (:import-from #:alexandria
                #:with-gensyms
                #:symbolicate)
  (:export #:config
           #:defconfig
           #:get-config
           #:set-config
           #:defprofile))

(in-package chameleon)

(define-condition null-profile-error (error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Profile is not set"))))

(defgeneric get-config (profile key))

(defmethod get-config ((profile (eql nil)) key)
  (error 'null-profile-error))

(defgeneric set-config (profile key value))

(defmethod set-config ((profile (eql nil)) key value)
  (error 'null-profile-error))

(defun make-keyword (thing)
  (alexandria:make-keyword (string-upcase thing)))

(defun config-item-to-slot (item)
  (let ((name (first item))
        (value (second item))
        (doc (third item)))
    (append (list name
                  :initarg (make-keyword name)
                  :initform value)
            (if doc (list :documentation doc) nil))))

(defun make-config-var-name (name)
  (symbolicate "*CONFIG-" (string-upcase name) "*"))

(defmacro defconfig (&body configs)
  (let* ((profile-sym (symbolicate '*profile*))
         (config-sym (symbolicate 'config))
         (configs configs))
    (with-gensyms (g-config g-value)
      `(progn
         (defvar ,profile-sym nil)

         (defclass ,config-sym ()
           ,(mapcar #'config-item-to-slot configs))

         ,@(mapcan (lambda (item)
                     `((defun ,(first item) ()
                         (let ((,g-config (chameleon:get-config
                                           ,profile-sym
                                           ',(first item))))
                           (if (functionp ,g-config)
                               (funcall ,g-config)
                               ,g-config)))

                       (defun (setf ,(first item)) (,g-value)
                         (chameleon:set-config ,profile-sym ',(first item) ,g-value))))
                   configs)))))

(defmacro defprofile (name &body configs)
  (let* ((name name)
         (configs configs)
         (config-var-name (make-config-var-name name)))
    (with-gensyms (g-profile g-key g-value)
      `(progn (defparameter ,config-var-name
                (funcall #'make-instance
                         ',(symbolicate 'config)
                         ,@(mapcan (lambda (pair)
                                     (assert (= (length pair) 2))
                                     (list (make-keyword (first pair))
                                           (second pair)))
                                   configs)))

              (defmethod chameleon:get-config ((,g-profile (eql ,name))
                                               ,g-key)
                (slot-value ,config-var-name ,g-key))

              (defmethod chameleon:set-config ((,g-profile (eql ,name))
                                               ,g-key
                                               ,g-value)
                (setf (slot-value ,config-var-name ,g-key) ,g-value))))))
