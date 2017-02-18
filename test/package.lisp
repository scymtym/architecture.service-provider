;;;; package.lisp --- Package definition for unit tests of the architecture.service-provider system.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:service-provider.test
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:fiveam

   #:service-provider)

  (:export
   #:run-tests)

  (:documentation
   "This package contains unit tests for the
    architecture.service-provider system"))

(cl:in-package #:service-provider.test)

;;; Root test suite and external interface

(def-suite service-provider
  :description
  "Root unit test suite for the architecture.service-provider
   system.")

(defun run-tests ()
  (run! 'service-provider))

;;; Generic test tools

(defmacro does-not-signal (condition &body body)
  `(handler-bind
       ((,condition (lambda (condition)
                      (fail "~@<Unexpected condition was signaled: ~A.~@:>"
                            condition))))
     (finishes ,@body)))

(defmacro with-clean-outer-compilation-unit (&body body)
  "Execute BODY keeping outer compilation-unit report(s) clean of
   compiler output."
  `(let ((*error-output* (make-broadcast-stream)))
     (with-compilation-unit (:override t)
       ,@body)))

(defmacro ignoring-compile-time-warnings ((condition) &body body)
  "Compile and execute BODY, ignoring conditions specified by
   CONDITION and keeping outer compilation-unit resport(s) clean of
   compiler output."
  `(funcall
    (with-clean-outer-compilation-unit
      (handler-bind ((,condition #'muffle-warning))
        (compile nil '(lambda () ,@body))))))

(defmacro compilation-signals (condition free-variables &body body)
  `(with-clean-outer-compilation-unit
     (signals ,condition (compile nil '(lambda ,free-variables ,@body)))))

(defmacro compilation-does-not-signal (condition free-variables &body body)
  `(with-clean-outer-compilation-unit
     (does-not-signal ,condition (compile nil '(lambda ,free-variables ,@body)))))

;;; Specific test tools

(defmacro with-cleaned-up-service ((name) &body body)
  "Ensure deletion of the service designated by NAME after execution
   of BODY."
  `(unwind-protect (progn ,@body)
     (setf (find-service ',name :if-does-not-exist nil) nil)))

(defmacro with-service ((name &body options) &body body)
  "Ensure deletion of the service designated by NAME after execution
   of BODY."
  `(with-cleaned-up-service (,name)
     (define-service ,name ,@options)
     ,@body))
