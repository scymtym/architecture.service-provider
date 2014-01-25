;;;; package.lisp --- Package definition for the architecture.service-provider system.
;;;;
;;;; Copyright (C) 2012, 2013, 2014 Jan Moringen
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

  ;; Class provider protocol
  (:export
   #:provider-class)

  ;; function provider protocol
  (:export
   #:provider-function)

  ;; Macros
  (:export
   #:define-service

   #:register-provider/class
   #:register-provider/function)

  (:documentation
   "This package contains functions and classes for defining, using
    and introspecting services and providers thereof.

    For services, the most important functions and macros are
    `find-service' and `define-service'.

    For providers, the most important functions and macros are
    `find-provider', `make-provider', `register-provider/class' and
    `register-provider/funtion'."))
