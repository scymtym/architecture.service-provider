;;;; service.lisp --- Service classes provided by the service-provider system.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:service-provider)

;;; `standard-service'

(defclass standard-service (name-mixin
                            documentation-mixin
                            provider-list-mixin
                            print-items:print-items-mixin)
  ((name :reader   service-name))
  (:documentation
   "This class provides a basic implementation of the service
    protocol. It can be used as a superclass for specialized service
    classes."))

(defmethod print-items:print-items append ((object standard-service))
  `((:name           ,(service-name object)               "~S")
    (:provider-count ,(length (service-providers object)) " (~D)"
                     ((:after :name)))))

(defmethod describe-object ((object standard-service) stream)
  (format stream "~A~@[~2&Providers:~&~{~A~^~&~}~]"
          object (service-providers object)))

(defmethod documentation ((slotd standard-service) (doc-type (eql t)))
  (service-documentation slotd))
