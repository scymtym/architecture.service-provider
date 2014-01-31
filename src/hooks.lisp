;;;; hooks.lisp --- Mixin for running hooks when providers of a service change.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:service-provider)

;;; Protocol

(defgeneric service-change-hook (service) ; TODO export?
  (:documentation
   "Return the change hook object for SERVICE."))

;;; `change-hook-mixin'

(defclass change-hook-mixin ()
  ((change-hook :type     list
                :initform '()
                :documentation
                "Stores a list of handlers which should be called when
                 providers are redefined, added or removed.

                 The lambda-list of handlers has to be compatible
                 to

                   (EVENT NAME PROVIDER)

                 where EVENT is one
                 of :provider-updated, :provider-added
                 or :provider-removed, NAME is a `provider-designator'
                 and PROVIDER is the affected provider object. "))
  (:documentation
   "This mixin class add to service classes a hook which is run
    whenever a provider of the service is redefined, added or
    removed."))

(defmethod service-change-hook ((service change-hook-mixin))
  (hooks:object-hook service 'change-hook))

(macrolet
    ((define-hook-method (name event)
       `(defmethod ,name :after ((service   change-hook-mixin)
                                 (name      t)
                                 (provider  t))
          (hooks:run-hook (service-change-hook service)
                          ,event name provider))))

  (define-hook-method update-provider :provider-updated)
  (define-hook-method add-provider    :provider-added)
  (define-hook-method remove-provider :provider-removed))
