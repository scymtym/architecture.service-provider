;;;; provider.lisp --- Builtin provider classes.
;;;;
;;;; Copyright (C) 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:service-provider)

;;; `class-provider'

(defclass class-provider (name-mixin)
  ((name  :reader   provider-name)
   (class :type     class
          :reader   provider-class
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
  (apply #'make-instance (provider-class provider) args))

(defmethod make-provider-form ((service     t)
                               (provider    class-provider)
                               (args        list)
                               (environment t))
  `(make-instance ,(class-name (provider-class provider)) ,@args))

(defmethod print-object ((object class-provider) stream)
  (let+ (((&structure-r/o provider- name class) object)
         (class-name (class-name class)))
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "~A~@[ [~S]~]"
              name (unless (eq name class-name) class-name)))))

(defmethod documentation ((slotd class-provider) (doc-type (eql t)))
  (documentation (provider-class slotd) t))

;;; `function-provider'

(defclass function-provider (name-mixin)
  ((name              :reader   provider-name)
   (function-name     :initarg  :function-name
                      :type     function-name
                      :reader   provider-function-name
                      :documentation
                      "Stores the name of the function providing the
                       service.")
   (lambda-expression :initarg  :lambda-expression
                      :type     lambda-expression
                      :reader   provider-lambda-expression
                      :documentation
                      "Stores the lambda-expression associated to the
                       function providing the service.")
   (function          :initarg  :function
                      :type     function
                      :reader   provider-function
                      :documentation
                      "Stores the function providing the service."))
  (:default-initargs
   :function-name (missing-required-initarg 'function-provider :function-name))
  (:documentation
   "Instances of this class provide a particular service by calling a
    given function and returning the result."))

(defmethod shared-initialize :before ((instance   function-provider)
                                      (slot-names t)
                                      &key
                                      (function-name     nil function-name-supplied?)
                                      (lambda-expression nil lambda-expression-supplied?)
                                      (function          nil function-supplied?))
  (when function-name-supplied?
    (check-type function-name function-name))
  (when lambda-expression-supplied?
    (check-type lambda-expression lambda-expression))
  (when function-supplied?
    (check-type function function)))

(defmethod shared-initialize :after ((instance   function-provider)
                                     (slot-names t)
                                     &key
                                     (function-name     nil function-name-supplied?)
                                     (lambda-expression nil lambda-expression-supplied?)
                                     (function          nil function-supplied?))
  (when function-name-supplied?
    (let ((function (if function-supplied?
                        function
                        (setf (slot-value instance 'function)
                              (coerce function-name 'function)))))
      (when (not lambda-expression-supplied?)
        (setf (slot-value instance 'lambda-expression)
              (function-lambda-expression function))))))

(defmethod make-provider ((service  t)
                          (provider function-provider)
                          &rest args)
  (apply (the function (provider-function provider)) args))

(defmethod make-provider-form ((service     t)
                               (provider    function-provider)
                               (args        list)
                               (environment t))
  ;; TODO try to obtain the function name here? what about lambdas?
  ;; we could have three slots: symbol, body, function with symbol,
  ;; body mutually exclusive?
  (let+ (((&structure-r/o
           provider- function-name lambda-expression) provider)
         ((&optional &ign lambda-list &body body) lambda-expression)
         (expand?          nil)
         (inline?          nil)
         (actually-expand? (and expand? lambda-list body))
         (actually-inline? inline?))
    (cond
      (actually-expand?
       `((lambda ,lambda-list ,@body) ,@args))
      (actually-inline?
       `(locally (declare (inline ,function-name))
          (,function-name ,@args)))
      (t
       `(,function-name ,@args)))))

(defmethod print-object ((object function-provider) stream)
  (let+ (((&structure-r/o provider- name function-name) object))
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "~A~@[ [~S]~]"
              name (unless (eq name function-name) function-name)))))

(defmethod documentation ((slotd function-provider) (doc-type (eql t)))
  (documentation (provider-function-name slotd) 'function))
