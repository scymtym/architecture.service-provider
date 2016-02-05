;;;; provider.lisp --- Unit tests for provider classes.
;;;;
;;;; Copyright (C) 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:service-provider.test)

(in-suite service-provider)

;;; `class-provider'

(defclass class-provider.mock-provider () ())

(test class-provider.print
  "Test printing a `class-provider' instance."

  (let+ (((&flet test-case (class-name name expect-class-name?)
            (with-service (foo)
              (%check-print-object-output
               (register-provider/class
                'foo name :class class-name)
               class-name name expect-class-name?)))))
    (test-case 'class-provider.mock-provider 'class-provider.mock-provider nil)
    (test-case 'class-provider.mock-provider 'bar                          t)))

;;; `function-provider'

(defun function-provider.mock-provider ())

(test function-provider.print
  "Test printing a `function-provider' instance."

  (let+ (((&flet test-case (function-name name expect-class-name?)
            (with-service (foo)
              (%check-print-object-output
               (register-provider/function
                'foo name :function function-name)
               function-name name expect-class-name?)))))
    (test-case 'function-provider.mock-provider 'function-provider.mock-provider nil)
    (test-case 'function-provider.mock-provider 'bar                             t)))

;;; Utilities

(defun %check-print-object-output
    (object object-name name expect-object-name?)
  (let* ((output               (princ-to-string object))
         (expected-name        (format nil "~A" name))
         (expected-object-name (format nil "[~S]" object-name)))
    (is (search expected-name output))
    (if expect-object-name?
        (is      (search expected-object-name output))
        (is (not (search expected-object-name output))))))