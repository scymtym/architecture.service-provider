;;;; provider.lisp --- Unit tests for provider classes.
;;;;
;;;; Copyright (C) 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:service-provider.test)

(in-suite service-provider)

;;; `class-provider'

(defclass class-provider.mock-provider () ()
  (:documentation
   "class-provider documentation"))

(test class-provider.construction
  "Test constructing `class-provider' instances."

  (let ((provider (make-instance 'class-provider
                                 :name  'class-provider.mock-provider
                                 :class 'class-provider.mock-provider)))
    (is (eq (find-class 'class-provider.mock-provider)
            (provider-class provider))))

  ;; Supplying a class instead of a class name should also work.
  (let* ((class    (find-class 'class-provider.mock-provider))
         (provider (make-instance 'class-provider
                                  :name  (class-name class)
                                  :class class)))
    (is (eq class (provider-class provider)))))

(test class-provider.construction.unsuitable-classes
  "Test constructing `class-provider' instances with unsuitable
   classes."

  (unwind-protect
       (progn
         ;; Not-defined class without explicitly allowing
         ;; forward-references signals an error.
         (signals error (make-instance 'class-provider
                                       :name  :no-such-class
                                       :class 'no-such-class))
         ;; No error when explicitly allowing forward references.
         (let ((provider (make-instance 'class-provider
                                        :name                    :no-such-class
                                        :class                   'no-such-class
                                        :allow-forward-reference t)))
           (is (typep (provider-class provider) 'class)))
         ;; forward-referenced class without explicitly allowing
         ;; forward-references signals an error.
         (signals error (make-instance 'class-provider
                                       :name  :no-such-class
                                       :class 'no-such-class)))
    (setf (find-class 'no-such-class) nil)))

(test class-provider.print
  "Test printing a `class-provider' instance."

  ;; This avoids compile-time style-warnings for using non-existent
  ;; services.
  (declare (notinline register-provider/class))

  (let+ (((&flet test-case (class-name name expect-class-name?)
            (with-service (foo)
              (%check-print-object-output
               (register-provider/class
                'foo name :class class-name)
               class-name name expect-class-name?)))))
    (test-case 'class-provider.mock-provider 'class-provider.mock-provider nil)
    (test-case 'class-provider.mock-provider 'bar                          t)))

(test class-provider.documentation
  "Test calling `documentation' on a `class-provider' instance."

  ;; This avoids compile-time style-warnings for using non-existent
  ;; services.
  (declare (notinline register-provider/class))

  (with-service (foo)
    (let ((provider (register-provider/class
                     'foo 'bar :class 'class-provider.mock-provider)))
      (is (equal "class-provider documentation" (documentation provider t))))))

;;; `function-provider'

(defun function-provider.mock-provider ()
  "function-provider documentation"
  nil)

(test function-provider.print
  "Test printing a `function-provider' instance."

  ;; This avoids compile-time style-warnings for using non-existent
  ;; services.
  (declare (notinline register-provider/function))

  (let+ (((&flet test-case (function-name name expect-class-name?)
            (with-service (foo)
              (%check-print-object-output
               (register-provider/function
                'foo name :function function-name)
               function-name name expect-class-name?)))))
    (test-case 'function-provider.mock-provider 'function-provider.mock-provider nil)
    (test-case 'function-provider.mock-provider 'bar                             t)))

(test function-provider.documentation
  "Test calling `documentation' on a `function-provider' instance."

  ;; This avoids compile-time style-warnings for using non-existent
  ;; services.
  (declare (notinline register-provider/function))

  (with-service (foo)
    (let ((provider (register-provider/function
                     'foo 'bar :function 'function-provider.mock-provider)))
      (is (equal "function-provider documentation"
                 (documentation provider t))))))

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
