;;;; service.lisp --- Unit tests for service classes.
;;;;
;;;; Copyright (C) 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:service-provider.test)

(in-suite service-provider)

(test standard-service.print
  "Test printing a `standard-service' instance."

  (with-service (foo)
    (let ((output (princ-to-string (find-service 'foo))))
      (is (search (format nil "~A" 'foo) output))
      (is (search "(0)"                  output)))))
