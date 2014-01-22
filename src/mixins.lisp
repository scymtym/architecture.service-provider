;;;; mixins.lisp --- Mixin classes used/provided by the architecture.service-provider system.
;;;;
;;;; Copyright (C) 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:service-provider)

;;; `name-mixin'

(defclass name-mixin ()
  ((name :initarg  :name
         :type     symbol
         :documentation
         "Stores the name of the service or provider."))
  (:default-initargs
   :name (missing-required-initarg 'name-mixin :name))
  (:documentation
   "TODO(jmoringe): document"))

;;; `documentation-mixin'

(defclass documentation-mixin ()
  ((documentation :initarg  :documentation
                  :type     (or null string)
                  :accessor service-documentation
                  :initform nil
                  :documentation
                  ""))
  (:documentation
   "TODO(jmoringe): document"))

;;; `provider-list-mixin'

(defclass provider-list-mixin ()
  ((providers :type     hash-table
              :reader   service-%providers
              :initform (make-hash-table :test #'eq)
              :documentation
              "Associate provider names to provider instances."))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod service-providers ((service provider-list-mixin))
  (hash-table-values (service-%providers service)))

(defmethod service-providers/alist ((service provider-list-mixin))
  (hash-table-alist (service-%providers service)))

(defmethod service-providers/plist ((service provider-list-mixin))
  (hash-table-plist (service-%providers service)))

(defmethod find-provider ((service  provider-list-mixin)
                          (provider symbol)
                          &key
                          if-does-not-exist)
  (declare (ignore if-does-not-exist))

  (values (gethash provider (service-%providers service))))

(defmethod (setf find-provider) ((new-value t)
                                 (service   provider-list-mixin)
                                 (provider  symbol)
                                 &key
                                 if-does-not-exist)
  (declare (ignore if-does-not-exist))

  (setf (gethash provider (service-%providers service))
        new-value))

(defmethod (setf find-provider) ((new-value (eql nil))
                                 (service   provider-list-mixin)
                                 (provider  symbol)
                                 &key
                                 if-does-not-exist)
  (let ((providers (service-%providers service)))
    (unless (gethash provider providers)
      (error-behavior-restart-case
          (if-does-not-exist
           (missing-provider-error
            :service    service
            :designator provider)
           :warning-condition missing-provider-warning)))

    (remhash provider providers)
    new-value))

;;; `synchronized-service-mixin'

(defclass synchronized-service-mixin ()
  ((lock :reader   service-%lock
         :initform (bt:make-recursive-lock "Service lock")
         :documentation
         "Stores a lock which can be used to protect the service
          against concurrent accesses."))
  (:documentation
   "This class is intended to be mixed into service classes which have
    to protect themselves against concurrent accesses using a lock."))

(defun call-with-locked-service (service thunk)
  (bt:with-recursive-lock-held ((service-%lock service))
    (funcall thunk)))

(defmacro with-locked-service ((service) &body body)
  `(call-with-locked-service ,service (lambda () ,@body)))

(macrolet
    ((define-synchronized-method (name args)
       (let+ ((args/parsed
               (substitute
                '(service synchronized-service-mixin) 'service args))
              ((&values &ign optional &ign keywords)
               (parse-ordinary-lambda-list
                args/parsed :allow-specializers t)))
         `(defmethod ,name :around ,args/parsed
            (declare (ignore ,@(mapcar #'first optional)
                             ,@(mapcar #'cadar keywords)))
            (with-locked-service (service) (call-next-method))))))

  (define-synchronized-method service-providers (service))
  (define-synchronized-method service-providers/alist (service))
  (define-synchronized-method service-providers/plist (service))
  (define-synchronized-method find-provider (service (provider symbol)
                                             &key if-does-not-exist))
  (define-synchronized-method (setf find-provider)
      ((new-value t) service (provider symbol)
       &key if-does-not-exist)))
