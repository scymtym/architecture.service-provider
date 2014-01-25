;;;; macros.lisp --- Macros provided by the architecture.service-provider system.
;;;;
;;;; Copyright (C) 2012, 2013, 2014 Jan Moringen
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

(defmacro define-provider ((service-name provider-name) spec &body options)
  "TODO(jmoringe): document"
  (check-type service-name  service-designator)
  (check-type provider-name provider-designator)

  (let+ (((name &rest initargs) spec)
         ((&plist-r/o
           (provider-class :provider-class
                           (symbolicate name '#:-provider)))
          (apply #'append options)))
    #+no (check-type provider-class symbol)

    `(register-provider ',service-name ',provider-name
                        ',provider-class (list ,@initargs))))

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

(define-register-function register-provider/function ((function-name provider-name))
  :required-initargs      (:function-name function-name)
  :default-provider-class function-provider
  :documentation
  "Register FUNCTION-NAME as the provider named PROVIDER-NAME of the
   service designated by SERVICE-NAME.

   PROVIDER-CLASS can be used to select the class of which the created
   provider should be an instance.

   The `cl:documentation' of FUNCTION-NAME is used as the
   documentation of the provider.")

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

(defmacro define-provider-function
    ((service-name provider-name
      &key
      (function-name (%provider-name->function-name
                      service-name provider-name)))
      args
     &body body)
  "TODO(jmoringe): document"
  `(progn
     (declaim (inline ,function-name))    ; save inlining data
     (defun ,function-name ,args ,@body)
     (declaim (notinline ,function-name)) ; but do not generally force inlining

     (register-provider/function
      ',service-name ',provider-name
      :function-name     ',function-name
      :lambda-expression '(lambda ,args ,@body))))

(defun %provider-name->function-name (service-name provider-name)
  (declare (type service-designator  service-name)
           (type provider-designator provider-name))
  (apply #'symbolicate '#:make
         (loop :for component :in (append (ensure-list provider-name)
                                          (ensure-list service-name))
            :collect '#:- :collect component)))
