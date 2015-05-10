;;;; macros.lisp --- Unit tests for macros of the architecture.service-provider system.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:service-provider.test)

(in-suite service-provider)

;;; `define-service' tests

(defclass specialized-service (standard-service)
  ())

(test macros.define-service.smoke
  "Smoke test for the `define-service' macro."

  (with-cleaned-up-service (foo)
    (define-service foo)
    (is (equal 'foo (service-name (find-service 'foo)))))

  ;; Test :documentation option.
  (with-cleaned-up-service (foo)
    (define-service foo
      (:documentation "foo"))
    (is (equal "foo" (documentation (find-service 'foo) t))))

  ;; Test :service-class option.
  (with-cleaned-up-service (foo)
    (define-service foo
      (:service-class specialized-service))
    (is (eq (find-class 'specialized-service)
            (class-of (find-service 'foo))))))

(test macros.define-service.redefinition.documentation
  "Test behavior of the `define-service' macro when service are
   redefined with different documentation."

  ;; Change documentation back and forth.
  (with-cleaned-up-service (foo)
    (define-service foo)

    (define-service foo
      (:documentation "foo"))
    (is (equal "foo" (documentation (find-service 'foo) t)))

    (define-service foo
      (:documentation nil))
    (is (equal nil (documentation (find-service 'foo) t)))))

(test macros.define-service.redefinition.change-class
  "Test behavior of the `define-service' macro when services are
   redefined with different classes."

  ;; Change class of service back and forth.
  (with-cleaned-up-service (foo)
    (define-service foo)

    (define-service foo
      (:service-class specialized-service))
    (is (eq (find-class 'specialized-service)
            (class-of (find-service 'foo)) ))

    (define-service foo)
    (is (eq (find-class 'standard-service)
            (class-of (find-service 'foo))))))

;;; `register-provider/class'

(defclass mock-provider ()
  ())

(defclass specialized-class-provider (class-provider)
  ())

(test macros.register-provider/class.smoke
  "Smoke test for the `register-provider/class' function."

  ;; Test registering and finding a simple provider.
  (with-service (foo)
    (register-provider/class 'foo :mock :class 'mock-provider)
    (is (equal :mock (provider-name (find-provider 'foo :mock)))))

  ;; Use the service object instead of its name.
  (with-service (foo)
    (let ((service (find-service 'foo)))
      (register-provider/class service :mock :class 'mock-provider)
      (is (equal :mock (provider-name (find-provider service :mock))))))

  ;; Test registering and finding a provider of a specialized class.
  (with-service (foo)
    (register-provider/class 'foo :mock
                             :provider-class 'specialized-class-provider
                             :class          'mock-provider)
    (is (equal :mock (provider-name (find-provider 'foo :mock))))
    (is (eq (find-class 'specialized-class-provider)
            (class-of (find-provider 'foo :mock) )))))

(test macros.register-provider/class.conditions
  "Test conditions signaled by the `register-provider/class' function."

  (signals missing-service-error
    (register-provider/class :no-such-service :does-not-matter
      :class 'mock-provider)))

;;; `register-provider/function'

(defun mock-provider ())

(defclass specialized-function-provider (function-provider)
  ())

(test macros.register-provider/function.smoke
  "Smoke test for the `register-provider/function' function."

  ;; Test registering and finding a simple provider.
  (with-service (foo)
    (register-provider/function 'foo :mock :function 'mock-provider)
    (is (equal :mock (provider-name (find-provider 'foo :mock)))))

  ;; Use the service object instead of its name.
  (with-service (foo)
    (let ((service (find-service 'foo)))
      (register-provider/function service :mock :function 'mock-provider)
      (is (equal :mock (provider-name (find-provider service :mock))))))

  ;; Test registering and finding a provider of a specialized class.
  (with-service (foo)
    (register-provider/function 'foo :mock
                                :provider-class 'specialized-function-provider
                                :function       'mock-provider)
    (is (equal :mock (provider-name (find-provider 'foo :mock))))
    (is (eq (find-class 'specialized-function-provider)
            (class-of (find-provider 'foo :mock) )))))

(test macros.register-provider/function.conditions
  "Test conditions signaled by the `register-provider/function'
   function."

  (signals missing-service-error
    (register-provider/function :no-such-service :does-not-matter
      :function 'mock-provider)))

#+no (define-provider (foo bar)
         (class :class 'bar))

#+no (define-provider (foo bar)
         (function :function 'bar))

#+no (define-provider-class (foo bar) ()
         ((slot :initarg :slot)))
