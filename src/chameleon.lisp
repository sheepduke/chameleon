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
- (ADD-PROFILE PROFILE) adds PROFILE to the profile list.
- (DELETE-PROFILE PROFILE) deletes PROFILE from the profile list.

Note that ADD-PROFILE and DELETE-PROFILE is low-level function and should not
be used directly."
  (with-gensyms (config-sym profile-sym profiles-sym)
    `(progn
       (unintern ',config-sym)
       (unintern ',profile-sym)

       (defvar ,profile-sym :default "Current profile.")
       (defvar ,profiles-sym '(:default) "All defined profiles.")
       (defvar ,config-sym (make-hash-table)
         "Place to hold all defined configurations.")

       ;; Generate code to initialize configuration items.
       (setf (gethash ,profile-sym ,config-sym) (make-hash-table))
       ,@(mapcar (lambda (pair)
                   `(setf (gethash ',(first pair) (gethash ,profile-sym ,config-sym))
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
                              (gethash ,name-sym (gethash :default ,config-sym))))))))

               (defun (setf ,(first pair)) (value)
                 (setf (gethash ',(first pair)
                                (gethash ,profile-sym ,config-sym))
                       value))))
          configs)

       ;; Generate functions for developers.
       (defun active-profile ()
         "Return currently active profile."
         ,profile-sym)

       (defun profiles ()
         "Return a list of available profiles."
         ,profiles-sym)

       (defun (setf active-profile) (profile)
         "Set the current active profile."
         (unless (member profile ,profiles-sym)
           (error "Profile ~a is not defined." profile))
         (setf ,profile-sym profile))

       (defun add-profile (profile)
         "Add given PROFILE to profile list."
         (push profile ,profiles-sym))

       (defun delete-profile (profile)
         "Delete PROFILE from profile list."
         (setf ,profiles-sym
               (delete profile ,profiles-sym)))

       (defun raw-configs ()
         "Return the raw configuration hash table."
         ,config-sym))))

(defmacro defprofile (profile-name)
  "Define a profile with name PROFILE-NAME.
A profile consists of a set of configurations."
  `(progn
     (delete-profile ,profile-name)
     (setf (gethash ,profile-name (raw-configs))
           (make-hash-table))
     ;; TODO set the value of configuration items.
     (add-profile ,profile-name)))

(defconfig
  (app-root "~/.silver-brain"
            "Root directory of this application.")
  (pair 1234))

(defprofile :develop
  )

(macroexpand
 '(defconfig
   (app-root "~/.silver-brain"
              "Root directory of this application.")
   (pair 1234)))
