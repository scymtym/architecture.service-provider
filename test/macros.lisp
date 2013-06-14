;;;; macros.lisp --- Unit tests for macros of the architecture.service-provider system.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:service-provider.test)

(in-suite service-provider)

;;; `define-service' tests

(defclass specialized-service (service-provider::standard-service)
  ())

(test macros.define-service.smoke

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
    (is (typep (find-service 'foo) 'specialized-service))))

(test macros.define-service.redefinition

  ;; Change documentation back and forth.
  (with-cleaned-up-service (foo)
    (define-service foo)
    (define-service foo
      (:documentation "foo"))
    (is (equal "foo" (documentation (find-service 'foo) t)))
    (define-service foo
      (:documentation nil))
    (is (equal nil (documentation (find-service 'foo) t))))

  ;; Change class of service.
  (with-cleaned-up-service (foo)
    (define-service foo)
    (define-service foo
      (:service-class specialized-service))
    (is (typep (find-service 'foo) 'specialized-service))))

;;; `register-provider/class'

(defclass mock-provider ()
  ())

(defclass specialized-provider (service-provider::class-provider)
  ())

(test macros.register-provider/class.smoke

  (with-service (foo)
    (register-provider/class 'foo :mock :class 'mock-provider)
    (find-provider 'foo :mock))

  (with-service (foo)
    (register-provider/class 'foo :mock
                             :provider-class 'specialized-provider
                             :class          'mock-provider)
    (is (typep (find-provider 'foo :mock) 'specialized-provider))))

;;; `register-provider/function'


#+no (define-provider (foo bar)
         (class :class 'bar))

#+no (define-provider (foo bar)
         (function :function 'bar))

#+no (define-provider-class (foo bar) ()
         ((slot :initarg :slot)))
