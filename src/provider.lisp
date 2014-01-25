;;;; provider.lisp --- Builtin provider classes.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:service-provider)

;;; `class-provider'

(defclass class-provider (name-mixin)
  ((name  :reader   provider-name)
   (class :type     class
          :reader   class-provider-class
          :documentation
          "Stores the class providing the service."))
  (:default-initargs
   :class (missing-required-initarg 'class-provider :class))
  (:documentation
   "Instances of this class provide a particular service by
    instantiating a given class."))

(defmethod shared-initialize :after ((instance   class-provider)
                                     (slot-names t)
                                     &key
                                     (class nil class-supplied?))
  (when class-supplied?
    (setf (slot-value instance 'class) (find-class class))))

(defmethod make-provider ((service  t)
                          (provider class-provider)
                          &rest args)
  (apply #'make-instance (class-provider-class provider) args))

(defmethod make-provider-form ((service  t)
                               (provider class-provider)
                               &rest args)
  `(make-instance ,(class-name (class-provider-class provider)) ,@args))

(defmethod print-object ((object class-provider) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (class-name (class-provider-class object)))))

(defmethod documentation ((slotd class-provider) (doc-type (eql t)))
  (documentation (class-provider-class slotd) t))

;;; `function-provider'

(defclass function-provider (name-mixin)
  ((name     :reader   provider-name)
   (function :type     (or symbol function)
             :reader   function-provider-function
             :documentation
             "Stores the function providing the service."))
  (:default-initargs
   :function (missing-required-initarg 'function-provider :function))
  (:documentation
   "Instances of this class provide a particular service by calling a
    given function and returning the result."))

(defmethod shared-initialize :after ((instance   function-provider)
                                     (slot-names t)
                                     &key
                                     (function nil function-supplied?))
  (when function-supplied?
    (check-type function (or symbol function))
    (setf (slot-value instance 'function) function)))

(defmethod make-provider ((service  t)
                          (provider function-provider)
                          &rest args)
  (apply (function-provider-function provider) args))

(defmethod make-provider-form ((service  t)
                               (provider function-provider)
                               &rest args)
  ;; TODO try to obtain the function name here? what about lambdas?
  ;; we could have three slots: symbol, body, function with symbol,
  ;; body mutually exclusive?
  `(,(function-provider-function provider) ,@args))

(defmethod print-object ((object function-provider) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (function-provider-function object))))

(defmethod documentation ((slotd function-provider) (doc-type (eql t)))
  (documentation (function-provider-function slotd) 'function))
