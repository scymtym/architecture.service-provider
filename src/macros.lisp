;;;; macros.lisp --- Macros provided by the architecture.service-provider system.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:service-provider)

(defmacro define-service (name &body options)
  "Define a service named NAME with additional aspects specified in
   OPTIONS.

   The following OPTIONS are accepted:

     (:service-class CLASS-NAME)

       Name of the class of the to-be-defined service. Defaults to
       `standard-service'.

     (:documentation STRING)

   If NAME already designates a service, the existing service object
   is destructively modified according to OPTIONS.

   The service definition is performed at compile, load and execute
   time to ensure availability in subsequent provider definitions
   and/or compilation of e.g. `find-service' calls."
  (check-type name service-designator)

  ;; We handle DOCUMENTATION explicitly so that redefinition without
  ;; the :documentation option will reset the documentation to nil
  ;; instead of retaining the current value.
  (let+ (((&plist-r/o
           (service-class :service-class 'standard-service)
           (documentation :documentation))
          (apply #'append options))
         (initargs (apply #'append
                          (remove-if
                           (rcurry #'member '(:service-class :documentation))
                           options :key #'first)))
         ((&with-gensyms service)))
    ;; Probably true, but I'm not sure
    ;; (check-type service-class symbol)

    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (find-service ',name)
             (if-let ((,service (find-service ',name :if-does-not-exist nil)))
               (change-class ,service ',service-class
                             :documentation ,documentation
                             ,@initargs)
               (make-instance ',service-class
                              :name          ',name
                              :documentation ,documentation
                              ,@initargs))))))

(defmacro define-provider ((service-name provider-name) spec &body options)
  "TODO(jmoringe): document"
  (check-type service-name  service-designator)
  (check-type provider-name provider-designator)

  (let+ (((name &rest initargs) spec)
         ((&plist-r/o
           (provider-class :provider-class
                           (symbolicate name '#:-provider)))
          (apply #'append options))
         ((&with-gensyms provider)))
    #+no (check-type provider-class symbol)

    `(setf (find-provider ',service-name ',provider-name)
           (if-let ((,provider (find-provider ',service-name ',provider-name
                                              :if-does-not-exist nil)))
             (change-class ,provider ',provider-class ,@initargs)
             (make-instance ',provider-class
                            :name ',provider-name
                            ,@initargs)))))

(macrolet
    ((define-register-function
         (name args
          &key
          required-initargs
          (default-provider-class (missing-required-argument
                                   :default-provider-class))
          documentation)
       `(defun ,name (service-name provider-name
                      &rest args
                      &key
                      (provider-class ',default-provider-class)
                      ,@args
                      &allow-other-keys)
          ,@(when documentation `(,documentation))
          (let ((initargs (append
                           (list :name provider-name)
                           (remove-from-plist
                            args :provider-class
                            ,@(mapcar #'car (plist-alist required-initargs))))))
            (setf (find-provider service-name provider-name)
                  (if-let ((provider (find-provider service-name provider-name
                                                    :if-does-not-exist nil)))
                    (apply #'change-class provider provider-class
                           ,@required-initargs initargs)
                    (apply #'make-instance provider-class
                           ,@required-initargs initargs)))))))

  (define-register-function register-provider/class ((class provider-name))
    :required-initargs      (:class class)
    :default-provider-class class-provider
    :documentation
    "Register CLASS as the provider named PROVIDER-NAME of the service
     designated by SERVICE-NAME.

     PROVIDER-CLASS can be used to select the class of which the
     created provider should be an instance.

     The `cl:documentation' of CLASS is used as the documentation of
     the provider.")

  (define-register-function register-provider/function ((function provider-name))
    :required-initargs      (:function function)
    :default-provider-class function-provider
    :documentation
    "Register FUNCTION as the provider named PROVIDER-NAME of the
     service designated by SERVICE-NAME.

     PROVIDER-CLASS can be used to select the class of which the
     created provider should be an instance.

     The `cl:documentation' of FUNCTION is used as the documentation
     of the provider."))

(defmacro define-provider-class ((service-name provider-name)
                                 direct-superclasses
                                 direct-slots
                                 &body options)
  "TODO(jmoringe): document"
  `(progn
     (defclass ,provider-name ,direct-superclasses
       ,direct-slots
       ,@options)

     #+no (define-provider (,service-name ,provider-name)
           (class ',provider-name))

     (register-provider/class ',service-name ',provider-name)))

(defmacro define-provider-function ((service-name provider-name) args
                                    &body body)
  "TODO(jmoringe): document"
  (let ((function-name (symbolicate '#:make- provider-name)))
    `(progn
       (defun ,function-name ,args ,@body)

       (register-provider/function
        ',service-name ',provider-name :function ',function-name))))
