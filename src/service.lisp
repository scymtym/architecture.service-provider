;;;; service.lisp --- Service classes provided by the service-provider system.
;;;;
;;;; Copyright (C) 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:service-provider)

;;; `standard-service'

(defclass standard-service (synchronized-service-mixin
                            name-mixin
                            documentation-mixin
                            provider-list-mixin)
  ((name :reader   service-name))
  (:documentation
   "TODO(jmoringe): document"))

(defmethod print-object ((object standard-service) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S (~D)"
            (service-name object)
            (length (service-providers object)))))

(defmethod describe-object ((object standard-service) stream)
  (format stream "~A~@[~2&Providers:~&~{~A~^~&~}~]"
          object (service-providers object)))

(defmethod documentation ((slotd standard-service) (doc-type (eql t)))
  (service-documentation slotd))
