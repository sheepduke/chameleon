(defpackage chameleon
  (:use #:cl)
  (:import-from #:alexandria
                #:with-gensyms
                #:symbolicate
                #:hash-table-keys)
  (:export #:defconfig
           #:defprofile))

(in-package chameleon)

(defmacro defconfig (&body configs)
  "Defines a configuration set. The CONFIGS is one or more lists, with each
list in the following form, very similar to DEFVAR:
(name default-value [docstring])

This macro then generates access function with the name defined above.

It also generates some general-purpose functions:
- (PROFILES) returns a list of defined profiles.
- (ACTIVE-PROFILE) returns the currently active profile.
- (SETF ACTIVE-PROFILE PROFILE) sets the active profile to PROFILE.

In addition, the following special variables can be used:
*PROFILE* => the current active profile.
*CONFIGS* => Hash table of defined configurations.

The key of *CONFIGS* is profile name and the value is a hash table containing
configuration items and their values. In normal cases, it should not be touched
directly.

All the symbols are NOT automatically exported.
"
  (let ((g-profile (symbolicate '*profile*))
        (g-config (symbolicate '*configs*)))
    `(progn
       (intern ,(string g-profile))
       (intern ,(string g-config))
       (defvar ,g-profile nil "Current profile.")
       (defvar ,g-config nil "Place to hold all defined profiles.")
       (setf ,g-profile nil)
       (setf ,g-config (make-hash-table))
       
       ;; Generate code to initialize configuration items.
       (setf (gethash ,g-profile ,g-config) (make-hash-table))
       ,@(mapcar (lambda (pair)
                   `(setf (gethash ',(first pair)
                                   (gethash ,g-profile ,g-config))
                          ,(second pair)))
                 configs)

       ;; Generate access function for each configuration item.
       ,@(mapcar
          (lambda (pair)
            `(progn
               (defun ,(first pair) ()
                 ,(if (third pair) (third pair) "")
                 ,(with-gensyms (g-name g-value g-foundp)
                    `(let ((,g-name ',(first pair)))
                       (multiple-value-bind (,g-value ,g-foundp)
                           (gethash ,g-name (gethash ,g-profile ,g-config))
                         (if ,g-foundp
                             (if (functionp ,g-value)
                                 (funcall ,g-value)
                                 ,g-value)
                             (gethash ,g-name (gethash nil ,g-config)))))))
               ,(with-gensyms (g-value)
                  `(defun (setf ,(first pair)) (,g-value)
                     (setf (gethash ',(first pair)
                                    (gethash ,g-profile ,g-config))
                           ,g-value)))))
          configs)

       ;; Generate functions for developers.
       (defun ,(symbolicate 'active-profile) ()
         "Return currently active profile."
         ,g-profile)
       
       (defun ,(symbolicate 'profiles) ()
         "Return a list of available profiles."
         (hash-table-keys ,g-config))
       
       ,(with-gensyms (g-profile-name)
          `(defun (setf ,(symbolicate 'active-profile)) (,g-profile-name)
             "Set the current active profile."
             (unless (member ,g-profile-name
                             (hash-table-keys ,g-config))
               (error "Profile ~a is not defined." ,g-profile-name))
             (setf ,g-profile ,g-profile-name)))
       nil)))

(defmacro defprofile (profile-name &body configs)
  "Define a profile with name PROFILE-NAME.
A profile consists of a set of configurations.
CONFIGS is one or more lists, with each list of the following form:
(key value)"
  (with-gensyms (g-hash-table g-raw-config)
    `(let ((,g-hash-table (make-hash-table))
           (,g-raw-config ,(symbolicate '*configs*)))
       (progn
         (remhash ,profile-name ,g-raw-config)
         (setf (gethash ,profile-name ,g-raw-config) ,g-hash-table)
         ,@(mapcar (lambda (pair)
                     `(setf (gethash ',(first pair) ,g-hash-table)
                            ,(second pair)))
                   configs)
         ,profile-name))))
