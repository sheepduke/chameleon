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

In addition, the following special variables are exposed:
*PROFILE* => the current active profile.
*CONFIGS* => Hash table of defined configurations.

The key of *CONFIGS* is profile name and the value is a hash table containing
configuration items and their values. In normal cases, it should not be touched
directly.
"
  (let ((profile-sym (symbolicate '*profile*))
        (config-sym (symbolicate '*configs*)))
    `(progn
       (intern ,(string profile-sym))
       (intern ,(string config-sym))
       (defvar ,profile-sym nil "Current profile.")
       (defvar ,config-sym nil "Place to hold all defined profiles.")
       (setf ,profile-sym nil)
       (setf ,config-sym (make-hash-table))
       
       ;; Generate code to initialize configuration items.
       (setf (gethash ,profile-sym ,config-sym) (make-hash-table))
       ,@(mapcar (lambda (pair)
                   `(setf (gethash ',(first pair)
                                   (gethash ,profile-sym ,config-sym))
                          ,(second pair)))
                 configs)

       ;; Generate access function for each configuration item.
       ,@(mapcar
          (lambda (pair)
            `(progn
               (defun ,(first pair) ()
                 ,(if (third pair) (third pair) "")
                 ,(with-gensyms (name-sym value-sym foundp-sym)
                    `(let ((,name-sym ',(first pair)))
                       (multiple-value-match
                           (gethash ,name-sym (gethash ,profile-sym ,config-sym))
                         ((,value-sym ,foundp-sym)
                          (if ,foundp-sym
                              ,value-sym
                              (gethash ,name-sym (gethash nil ,config-sym))))))))
               (export ',(first pair))

               ,(with-gensyms (value-sym)
                  `(defun (setf ,(first pair)) (,value-sym)
                     (setf (gethash ',(first pair)
                                    (gethash ,profile-sym ,config-sym))
                           ,value-sym)))))
          configs)

       ;; Generate functions for developers.
       (defun ,(symbolicate 'active-profile) ()
         "Return currently active profile."
         ,profile-sym)

       (defun ,(symbolicate 'profiles) ()
         "Return a list of available profiles."
         (hash-table-keys ,config-sym))

       ,(with-gensyms (profile-name-sym)
          `(defun (setf ,(symbolicate 'active-profile)) (,profile-name-sym)
             "Set the current active profile."
             (unless (member ,profile-name-sym
                             (hash-table-keys ,config-sym))
               (error "Profile ~a is not defined." ,profile-name-sym))
             (setf ,profile-sym ,profile-name-sym)))
       nil)))

(defmacro defprofile (profile-name &body configs)
  "Define a profile with name PROFILE-NAME.
A profile consists of a set of configurations.
CONFIGS is one or more lists, with each list of the following form:
(key value)"
  (with-gensyms (hash-table-sym raw-config-sym)
    `(let ((,hash-table-sym (make-hash-table))
           (,raw-config-sym ,(symbolicate '*configs*)))
       (progn
         (remhash ,profile-name ,raw-config-sym)
         (setf (gethash ,profile-name ,raw-config-sym) ,hash-table-sym)
         ,@(mapcar (lambda (pair)
                     `(setf (gethash ',(first pair) ,hash-table-sym)
                            ,(second pair)))
                   configs)
         ,profile-name))))
