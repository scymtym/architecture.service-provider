;;;; package.lisp --- Package definition for the architecture.service-provider system.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:service-provider
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions)

  ;; Conditions
  (:export
   #:service-provider-condition     ; superclass of all conditions

   #:missing-service-designator

   #:missing-service-warning
   #:missing-service-error

   #:missing-provider-service
   #:missing-provider-designator

   #:missing-provider-warning
   #:missing-provider-error)

  ;; Restarts
  (:export
   #:retry)

  ;; Service Protocol
  (:export
   #:service-name
   #:service-providers
   #:service-providers/alist
   #:service-providers/plist

   #:find-service)                  ; also setf

  ;; Provider Protocol
  (:export
   #:provider-name

   #:find-provider                  ; also setf
   #:make-provider)

  ;; Macros
  (:export
   #:define-service

   #:register-provider/class
   #:register-provider/function)

  (:documentation
   "TODO"))
