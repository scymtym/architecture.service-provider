;;;; types.lisp --- Types used/provided by the service-provider system.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:service-provider)

(deftype service-designator ()
  "A symbol designating a service."
  '(and symbol (not null)))

(deftype provider-designator ()
  "A symbol designating a service provider."
  '(and symbol (not null)))
