(defpackage chameleon
  (:use #:cl)
  (:import-from #:alexandria
                #:if-let
                #:with-gensyms
                #:symbolicate)
  (:export #:config
           #:defconfig
           #:defprofile
           #:eval-once))

(in-package chameleon)

(define-condition null-profile-error (error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Profile is not set"))))

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
         (config-var-sym (symbolicate '*config*))
         (config-sym (symbolicate 'config))
         (value-sym (symbolicate 'value))
         (configs configs))
    `(progn
       ;; Generate *profile* variable.
       (defvar ,profile-sym nil
         "The current profile.")

       ;; Generate *config* variable.
       (defvar ,config-var-sym nil
         "The configuration instance for current profile.")

       ;; Generate 'config class.
       (defclass ,config-sym ()
         ,(mapcar #'config-item-to-slot configs))

       ;; Generate generic function switch-profile.
       (defgeneric ,(symbolicate 'switch-profile) (,(symbolicate 'profile))
         (:documentation "Switch to new PROFILE."))

       ;; Generate access functions.
       ,@(mapcan (lambda (item)
                   `((declaim (inline ,(first item)))
                     (defun ,(first item) ()
                       (let ((,config-sym (slot-value ,config-var-sym
                                                      ',(first item))))
                         (if (functionp ,config-sym)
                             (funcall ,config-sym)
                             ,config-sym)))

                     (defun (setf ,(first item)) (,value-sym)
                       (setf (slot-value ,config-var-sym ',(first item))
                             ,value-sym))))
                 configs))))

(defmacro defprofile (name &body configs)
  "Defines a profile with given NAME. CONFIGS is one or more lists,
with each following this pattern: (name value).

The evaluation rule of value follows DEFCONFIG."
  (let* ((name name)
         (configs configs)
         (config-var-name (symbolicate "*CONFIG-" (string-upcase name) "*"))
         (profile-sym (symbolicate 'profile)))
    `(progn
       ;; Generate 'config instance.
       (defparameter ,config-var-name
         (funcall #'make-instance
                  ',(symbolicate 'config)
                  ,@(mapcan (lambda (pair)
                              (assert (= (length pair) 2))
                              (list (make-keyword (first pair))
                                    (second pair)))
                            configs)))

       ;; Generate switch-profile method.
       (defmethod ,(symbolicate 'switch-profile) ((,profile-sym (eql ,name)))
         (setf ,(symbolicate '*config*) ,config-var-name)
         (setf ,(symbolicate '*profile*) ,profile-sym)))))

(defmacro eval-once (&body body)
  "Defines a closure to evaluate BODY for only once."
  (with-gensyms (g-value)
    `(let (,g-value)
       (lambda ()
         (if ,g-value ,g-value (setf ,g-value (progn ,@body)))))))
