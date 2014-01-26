;;;; mixins.lisp --- Mixin classes used/provided by the architecture.service-provider system.
;;;;
;;;; Copyright (C) 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:service-provider)

;;; `name-mixin'

(defclass name-mixin ()
  ((name :initarg  :name
         :type     provider-designator
         :documentation
         "Stores the name of the service or provider."))
  (:default-initargs
   :name (missing-required-initarg 'name-mixin :name))
  (:documentation
   "This class is intended to be mixed into service or provider
    classes which have an associated name."))

;;; `documentation-mixin'

(defclass documentation-mixin ()
  ((documentation :initarg  :documentation
                  :type     (or null string)
                  :accessor service-documentation
                  :initform nil
                  :documentation
                  "Stores nil or a documentation string."))
  (:documentation
   "This class is intended to be mixed into service or provider
    classes which support associated documentation strings."))

;;; `provider-list-mixin'

(defclass provider-list-mixin ()
  ((providers :type     hash-table
              :reader   service-%providers
              :initform (make-hash-table :test #'equal)
              :documentation
              "Associate provider names to provider instances."))
  (:documentation
   "This class is intended to be mixed service classes which have to
    store a list of providers."))

(defmethod service-providers ((service provider-list-mixin))
  (hash-table-values (service-%providers service)))

(defmethod service-providers/alist ((service provider-list-mixin))
  (hash-table-alist (service-%providers service)))

(defmethod service-providers/plist ((service provider-list-mixin))
  (hash-table-plist (service-%providers service)))

(macrolet
    ((define-find-provider (specializer checked-type)
       `(defmethod find-provider ((service  provider-list-mixin)
                                  (provider ,specializer)
                                  &key
                                  if-does-not-exist)
          (declare (ignore if-does-not-exist))
          (check-type provider ,checked-type)

          (values (gethash provider (service-%providers service))))))

  (define-find-provider symbol provider-designator/symbol)
  (define-find-provider cons   provider-designator/cons))

(macrolet
    ((define-setf-find-provider (specializer checked-type)
       `(defmethod (setf find-provider) ((new-value t)
                                         (service   provider-list-mixin)
                                         (provider  ,specializer)
                                         &key
                                         if-does-not-exist)
          (declare (ignore if-does-not-exist))
          (check-type provider ,checked-type)

          (setf (gethash provider (service-%providers service))
                new-value))))

  (define-setf-find-provider symbol provider-designator/symbol)
  (define-setf-find-provider cons   provider-designator/cons))

(macrolet
    ((define-setf-find-provider (specializer checked-type)
       `(defmethod (setf find-provider) ((new-value (eql nil))
                                         (service   provider-list-mixin)
                                         (provider  ,specializer)
                                         &key
                                         if-does-not-exist)
          (check-type provider ,checked-type)

          (%remove-provider provider service if-does-not-exist)
          new-value)))

  (define-setf-find-provider symbol provider-designator/symbol)
  (define-setf-find-provider cons   provider-designator/cons))

(defun %remove-provider (provider service if-does-not-exist)
  (declare (type provider-list-mixin service))

  (let ((providers (service-%providers service)))
    (unless (gethash provider providers)
      (error-behavior-restart-case
          (if-does-not-exist
           (missing-provider-error
            :service    service
            :designator provider)
           :warning-condition missing-provider-warning)))
    (remhash provider providers)))
