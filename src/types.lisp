;;;; types.lisp --- Types used/provided by the service-provider system.
;;;;
;;;; Copyright (C) 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:service-provider)

;;;

(deftype service-designator ()
  "A symbol designating a service."
  '(and symbol (not null)))

(deftype provider-designator/symbol ()
  '(and symbol (not null)))

(deftype provider-designator/cons ()
  '(cons provider-designator/symbol (not null)))

(deftype provider-designator ()
  "A symbol designating a service provider."
  '(or provider-designator/symbol provider-designator/cons))

;;;

(deftype function-name ()
  "TODO"
  #+sbcl '(satisfies sb-int:legal-fun-name-p)
  #-sbcl '(or (and symbol (not null)) (cons symbol)))

(deftype lambda-expression ()
  "TODO"
  '(cons symbol (cons list)))
