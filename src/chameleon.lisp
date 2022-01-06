(defpackage chameleon
  (:use #:cl)
  (:import-from #:alexandria
                #:if-let
                #:with-gensyms
                #:symbolicate)
  (:export #:config
           #:defconfig
           #:get-config
           #:set-config
           #:defprofile
           #:eval-once))

(in-package chameleon)

(define-condition null-profile-error (error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Profile is not set"))))

(defgeneric get-config (profile key)
  (:documentation "Get configuration instance with given PROFILE and KEY."))

(defmethod get-config ((profile (eql nil)) key)
  (error 'null-profile-error))

(defgeneric set-config (profile key value)
  (:documentation "Set configuration value with given PROFILE, KEY and VALUE."))

(defmethod set-config ((profile (eql nil)) key value)
  (error 'null-profile-error))

(defun make-keyword (thing)
  "Make a keyword out of given string THING. The keyword is guaranteed
to be upper case."
  (alexandria:make-keyword (string-upcase thing)))

(defun config-item-to-slot (item)
  "Convert given ITEM to CLOS slot. ITEM should be the a 2 or 3 length
pair in config definition."
  (let ((name (first item))
        (value (second item))
        (doc (third item)))
    (append (list name
                  :initarg (make-keyword name)
                  :initform value)
            (if doc (list :documentation doc) nil))))

(defmacro defconfig (&body configs)
  "Defines a configuration. The body CONFIGS contains multiple items,
with each following this pattern:
  (name initial-value &optional docstring)

While calling this macro, all the given names, initial-values and
docstrings are evaluated during macro expansion.

Beneath the surface, DEFCONFIG actually generates the following stuff:

1. A variable *PROFILE*. It stores the current profile which is
   usually a keyword.

2. A class named CONFIG. Each item maps to a slot definition:
   - name maps to slot name.
   - initial-value maps to :initform argument.
   - docstring maps to :documentation property of the slot.

3. A zero-arity function (and its setf version) for each name. The
   function returns corresponding value according to *PROFILE*. When
   the value itself is a function, it is called every time and the
   value is returned. Otherwise, the value is directly returned.

   This is useful for evaluating an expression every time.

A typical example is:
(defconfig
  (server-port 5001 \"The server port.\")
  (app-dir \"/tmp\"))"
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
                         ,(if-let (docstring (third item))
                            docstring
                            "")
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
  "Defines a profile with given NAME. CONFIGS is one or more lists,
with each following this pattern: (name value).

The evaluation rule of value follows DEFCONFIG."
  (let* ((name name)
         (configs configs)
         (config-var-name (symbolicate "*CONFIG-" (string-upcase name) "*")))
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

(defmacro eval-once (&body body)
  "Defines a closure to evaluate BODY for only once."
  (with-gensyms (g-value)
    `(let (,g-value)
       (lambda ()
         (if ,g-value ,g-value (setf ,g-value (progn ,@body)))))))
