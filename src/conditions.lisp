;;;; conditions.lisp --- Conditions used by the architecture.service-provider system.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:service-provider)

(define-condition service-provider-condition (condition)
  ()
  (:documentation
   "Superclass of all conditions signaled by the service-provider
    system."))

(define-condition missing-service-condition (service-provider-condition)
  ((designator :initarg  :designator
               :type     service-designator
               :reader   missing-service-designator
               :documentation
               "The designator for which no class could be found."))
  (:default-initargs
   :designator (missing-required-initarg
                'missing-service-condition :designator))
  (:report
   (lambda (condition stream)
     (format stream "~@<No service is known for the designator ~S.~@:>"
             (missing-service-designator condition))))
  (:documentation
   "Subclasses of this are signaled when services specified by
    designators cannot be found."))

(define-condition missing-service-warning (style-warning
                                           missing-service-condition)
  ()
  (:documentation
   "This warning is signaled when a service specified by a designator
    cannot be found."))

(define-condition missing-service-error (error
                                         missing-service-condition)
  ()
  (:documentation
   "Subclasses of this are signaled when services specified by
    designators cannot be found."))

(define-condition missing-provider-condition (service-provider-condition)
  ((service    :initarg  :service
               :reader   missing-provider-service
               :documentation
               "Stores the service in which the specified provider
                could not be found.")
   (designator :initarg  :designator
               :type     provider-designator
               :reader   missing-provider-designator
               :documentation
               "The designator for which no provider could be
                found."))
  (:default-initargs
   :service    (missing-required-initarg
                'missing-provider-condition :service)
   :designator (missing-required-initarg
                'missing-provider-condition :designator))
  (:report
   (lambda (condition stream)
     (format stream "~@<No provider of service ~A is known for the ~
                     designator ~S.~@:>"
             (missing-provider-service    condition)
             (missing-provider-designator condition))))
  (:documentation
   "Subclasses of this are signaled when providers specified by
    designators cannot be found within gives services."))

(define-condition missing-provider-warning (style-warning
                                            missing-provider-condition)
  ()
  (:documentation
   "This warning is signaled when a provider of a certain service is
    specified as a designator but cannot be found."))

(define-condition missing-provider-error (error
                                          missing-provider-condition)
  ()
  (:documentation
   "This error is signaled when a provider of a certain service is
    specified as a designator but cannot be found."))
