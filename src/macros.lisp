;;;; macros.lisp --- Macros provided by the architecture.service-provider system.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:service-provider)

;;; Service registration

(defun register-service (name service-class initargs &optional documentation)
  (check-type name service-designator)

  (setf (find-service name)
        (if-let ((service (find-service name :if-does-not-exist nil)))
          (apply #'change-class service service-class
                 :documentation documentation
                 initargs)
          (apply #'make-instance service-class
                 :name          name
                 :documentation documentation
                 initargs))))

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
  (let+ ((options/plist (reduce #'append options))
         ((&plist-r/o (service-class :service-class 'standard-service)
                      (documentation :documentation))
          options/plist)
         (initargs (remove-from-plist
                    options/plist :service-class :documentation)))
    ;; Probably true, but I'm not sure
    ;; (check-type service-class symbol)

    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (register-service
        ',name ',service-class (list ,@initargs) ,documentation))))

;;; Provider registration

(defun register-provider (service-name provider-name provider-class initargs)
  (check-type service-name  service-designator)
  (check-type provider-name provider-designator)

  (setf (find-provider service-name provider-name)
        (if-let ((provider (find-provider service-name provider-name
                                          :if-does-not-exist nil)))
          (apply #'change-class provider provider-class initargs)
          (apply #'make-instance provider-class
                 :name provider-name
                 initargs))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-register-function
      (name args
       &key
       required-initargs
       (default-provider-class (missing-required-argument
                                :default-provider-class))
       documentation)
    "Define a function named NAME with a lambda-list of the form

       (SERVICE-NAME PROVIDER-NAME . KEYWORD-PARAMETERS)

     which registers service providers for the given service +
     provider combinations. ARGS has to be a list of elements the form

       (NAME DEFAULT)

     . As part of KEYWORD-PARAMETERS, the defined function accepts
     a :provider-class keyword parameter which defaults to
     DEFAULT-PROVIDER-CLASS. When instantiated, the provider class
     will receive as initargs REQUIRED-INITARGS, .

     If supplied, DOCUMENTATION will be used as the documentation
     string of the defined function."
    `(defun ,name (service-name provider-name
                   &rest args
                   &key
                   (provider-class ',default-provider-class)
                   ,@args
                   &allow-other-keys)
       ,@(when documentation `(,documentation))
       (check-type service-name  service-designator)
       (check-type provider-name provider-designator)

       (let ((initargs (list*
                        ,@required-initargs
                        (remove-from-plist
                         args :provider-class
                         ,@(mapcar #'car (plist-alist required-initargs))))))

         (register-provider
          service-name provider-name provider-class initargs)))))

(define-register-function register-provider/class ((class provider-name))
  :required-initargs      (:class class)
  :default-provider-class class-provider
  :documentation
  "Register CLASS as the provider named PROVIDER-NAME of the service
   designated by SERVICE-NAME.

   PROVIDER-CLASS can be used to select the class of which the created
   provider should be an instance.

   The `cl:documentation' of CLASS is used as the documentation of the
   provider.")

(define-register-function register-provider/function ((function provider-name))
  :required-initargs      (:function function)
  :default-provider-class function-provider
  :documentation
  "Register FUNCTION as the provider named PROVIDER-NAME of the
   service designated by SERVICE-NAME.

   PROVIDER-CLASS can be used to select the class of which the created
   provider should be an instance.

   The `cl:documentation' of FUNCTION is used as the documentation of
   the provider.")
