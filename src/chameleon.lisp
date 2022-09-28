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

(defvar *profile-type* :variable
  "Type of form to use for profile definitions (:VARIABLE or :PARAMETER).

This special controls whether the variable that contains a configuration
profile is defined using DEFVAR or DEFPARAMETER. When set to :VARIABLE,
DEFPROFILE will define the profile using DEFVAR. When the form is re-
evaluated the profile will not be updated, according to the semantics
of DEFVAR.

When set to :PARAMETER, DEFPROFILE will define profile variables using
DEFPARAMETER, updating the active instance of the profile when the form
is re-evaluated. This is useful for interactive development when modifying
configuration profiles in the running image.

The default value of *PROFILE-TYPE* is :VARIABLE.")

(defvar *switch-profiles* nil
  "Automatically switch to a profile when defining it (generalised boolean).

This variable controls whether evaluating a DEFPROFILE form activates the
new profile using a call to SWITCH-PROFILE. This facilitates interactive
development by allowing redefining and activating a profile as a single
operation. When a true value, profiles will be activated when evaluating
a DEFPROFILE form, when NIL the profile will still be defined, but not
activated.

This behaviour respects the setting of *PROFILE-TYPE*, i.e. to redefine and
activate an updated profile, *PROFILE-TYPE* should be :PARAMETER and
 *SWITCH-PROFILES* should be T.

The default value of *SWITCH-PROFILES* is NIL.")

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

(defun make-config-var-name (name)
  (symbolicate "*CONFIG-" (string-upcase name) "*"))

(defmacro defconfig (&body configs)
  "Defines a configuration. The body CONFIGS contains multiple items,
with each following this pattern:
  (name initial-value &optional docstring)

While calling this macro, all the given names, initial-values and
docstrings are evaluated during macro expansion.

Beneath the surface, DEFCONFIG actually generates the following stuff:

1. A variable *PROFILE*. It stores the current profile which is
   desired to be a keyword.

2. A variable *CONFIG*. It stores the current configuration instance.

3. A class named CONFIG. Each item maps to a slot definition:
   - name maps to slot name.
   - initial-value maps to :initform argument.
   - docstring maps to :documentation property of the slot.

4. A zero-arity inline function (and its setf version) for each
   name. The function returns configuration value of current
   profile. When the value itself is a function, it is called every
   time and the value is returned. Otherwise, the value is directly
   returned.

5. Some other helper functions and macros:
   - with-profile

A typical example is:
(defconfig
  (server-port 5001 \"The server port.\")
  (app-dir \"/tmp\"))"
  (let* ((profile-var-sym (symbolicate '*profile*))
         (profile-sym (symbolicate 'profile))
         (config-var-sym (symbolicate '*config*))
         (config-sym (symbolicate 'config))
         (value-sym (symbolicate 'value))
         (configs configs)
         (package *package*))
    `(progn
       ;; Generate *profile* variable.
       (defvar ,profile-var-sym nil
         "The current profile.")

       ;; Generate *config* variable.
       (defvar ,config-var-sym nil
         "The configuration instance for current profile.")

       ;; Generate with-profile macro.
       (defmacro ,(symbolicate 'with-profile)
           (,profile-sym &body ,(symbolicate 'body))
         "Temporally switch to given PROFILE and evaluate BODY."
         `(let ((,',(intern "*PROFILE*" package) ,,profile-sym)
                (,',(intern "*CONFIG*" package)
                  ,(eval-config-var ,profile-sym ,package)))
            ,@,(symbolicate 'body)))

       ;; Generate 'config class.
       (defclass ,config-sym ()
         ,(mapcar #'config-item-to-slot configs))

       ;; Generate generic function switch-profile.
       (defgeneric ,(symbolicate 'switch-profile) (,profile-sym)
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

The evaluation rule of value follows DEFCONFIG.

It generates a variable *CONFIG-<NAME>* and a method SWITCH-PROFILE.

You may provide :before, :around or :after methods to SWITCH-PROFILE
to insert some code."
  (let* ((name name)
         (configs configs)
         (config-var-name (make-config-var-name name))
         (profile-sym (symbolicate 'profile)))
    `(progn
       ;; Generate 'config instance.
       (,(ecase *profile-type*
           (:parameter 'defparameter)
           (:variable 'defvar))
        ,config-var-name
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
         (setf ,(symbolicate '*profile*) ,profile-sym))

       ;; Optionally switch to the newly defined profile.
       ,(when *switch-profiles*
          `(,(symbolicate 'switch-profile) ,name)))))

(defmacro eval-once (&body body)
  "Defines a closure to evaluate BODY for only once."
  (with-gensyms (g-value)
    `(let (,g-value)
       (lambda ()
         (if ,g-value ,g-value (setf ,g-value (progn ,@body)))))))

(defun eval-config-var (profile package)
  (eval (find-symbol
         (string-upcase (format nil "*CONFIG-~a*" profile))
         package)))
