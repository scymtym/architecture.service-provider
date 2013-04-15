;;;; service.lisp --- Service classes provided by the service-provider system.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:service-provider)

;;; `standard-service'

(defclass standard-service (name-mixin
                            documentation-mixin
                            provider-list-mixin)
  ()
  (:documentation
   "TODO(jmoringe): document"))

(defmethod documentation ((slotd    standard-service)
                          (doc-type t))
  (format nil "~:[Not documented.~;~:*~A~]~@[~2&Providers:~&~{~A~^~&~}~]"
          (service-documentation slotd)
          (service-providers slotd)))

(defmethod print-object ((object standard-service) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S (~D)"
            (service-name object)
            (length (service-providers object)))))
