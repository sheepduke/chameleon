(in-package chameleon)

(defmacro defconfig (&body configs)
  "Defines a configuration set. The CONFIGS is one or more lists, with each
list in the following form, very similar to DEFVAR:
(name default-value [docstring])

This macro generates access functions with name given above."
  (with-gensyms (config-sym profile-sym profile-syms)
    `(progn
       (unintern ',config-sym)
       (unintern ',profile-sym)

       (defvar ,profile-sym :default "Current profile.")
       (defvar ,profile-syms '(:default) "All defined profiles.")
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
                              ;; (gethash ,name-sym (gethash :default ,config-sym))
                              "999"))))))
               
               (defun (setf ,(first pair)) (value)
                 (setf (gethash ',(first pair)(maphash-keys )
                                (gethash ,profile-sym ,config-sym))
                       value))))
          configs)

       ;; Generate functions for developers.
       (defun profile ()
         "Return currently active profile."
         ,profile-sym)

       (defun profiles ()
         "Return a list of available profiles."
         ,profile-syms)

       (defun (setf profile) (profile)
         "Set the current active profile."
         (unless (member profile ,profile-syms)
           (error "Profile ~a is not defined." profile))
         (setf ,profile-sym profile))

       (defun add-profile (profile)
         (push profile ,profile-syms)))))

(defmacro defprofile (name)
  `(add-profile ,name)
  )

(defconfig
  (app-root "~/.silver-brain"
            "Root directory of this application.")
  (pair 1234))

(defprofile :develop
  )

;; (macroexpand
;;  '(defconfig
;;    (app-root "~/.silver-brain"
;;               "Root directory of this application.")
;;    (pair 1234)))

(macroexpand-1
 '(defprofile :develop ))

;; (add-profile :develop)
;; (setf (profile) :develop)
;; (app-root)

;; (setf (profile) :develop)
