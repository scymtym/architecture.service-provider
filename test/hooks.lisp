;;;; hooks.lisp --- Unit tests for the hooks extension.
;;;;
;;;; Copyright (C) 2014, 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:service-provider.hooks.test
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:fiveam

   #:service-provider)

  (:import-from #:service-provider.test
   #:with-service)

  (:export
   #:run-tests)

  (:documentation
   "Contains unit tests for the hooks extension."))

(cl:in-package #:service-provider.hooks.test)

(def-suite service-provider.hooks)
(in-suite service-provider.hooks)

(defun run-tests ()
  (run! 'service-provider.hooks))

(defmacro with-hook-call-recording ((service calls-var) &body body)
  `(let ((,calls-var '()))
     (hooks:add-to-hook (service-provider::service-change-hook (find-service ,service))
                        (lambda (&rest args)
                          (appendf ,calls-var (list args))))
     ,@body))

(defmacro check-call ((calls-var count) expected)
  (once-only (count)
    (let+ (((expected-event expected-name expected-type) expected))
      `(let+ (((event name provider) (nth (1- ,count) ,calls-var)))
         (is (length= ,count ,calls-var))
         (is (eq ',expected-event event))
         (is (equal ',expected-name name))
         (is (typep provider ',expected-type))))))

;; TODO make a builtin class for this?
(defclass hooks.smoke.service (standard-service
                               service-provider::change-hook-mixin)
  ())

(test hooks.smoke/function
  "Smoke test for hooks provided by the `change-hook-mixin' service
   mixin class in combination with a function provider."

  ;; This avoids compile-time style-warnings for using non-existent
  ;; services.
  (declare (notinline register-provider/function
                      find-provider (setf find-provider)))

  (with-service (:mock (:service-class hooks.smoke.service))
    (with-hook-call-recording (:mock calls)

      (register-provider/function :mock 'foo)
      (check-call (calls 1) (:provider-added foo function-provider))

      (register-provider/function :mock 'foo)
      (check-call (calls 2) (:provider-updated foo function-provider))

      (register-provider/function :mock 'foo)
      (check-call (calls 3) (:provider-updated foo function-provider))

      (setf (find-provider :mock 'foo) nil)
      (check-call (calls 4) (:provider-removed foo function-provider))

      ;; Attempting to remove a non-existent provider should not run
      ;; hooks.
      (setf (find-provider :mock 'bar :if-does-not-exist nil) nil)
      (check-call (calls 4) (:provider-removed foo function-provider)))))

(defclass hooks.smoke.provider-class () ())

(test hooks.smoke/class
  "Smoke test for hooks provided by the `change-hook-mixin' service
   mixin class in combination with a class provider."

  ;; This avoids compile-time style-warnings for using non-existent
  ;; services.
  (declare (notinline register-provider/class
                      find-provider (setf find-provider)))

  (with-service (:mock (:service-class hooks.smoke.service))
    (with-hook-call-recording (:mock calls)

      (register-provider/class :mock 'hooks.smoke.provider-class)
      (check-call (calls 1)
        (:provider-added hooks.smoke.provider-class class-provider))

      (register-provider/class :mock 'hooks.smoke.provider-class)
      (check-call (calls 2)
        (:provider-updated hooks.smoke.provider-class class-provider))

      (register-provider/class :mock 'hooks.smoke.provider-class)
      (check-call (calls 3)
        (:provider-updated hooks.smoke.provider-class class-provider))

      (setf (find-provider :mock 'hooks.smoke.provider-class) nil)
      (check-call (calls 4)
        (:provider-removed hooks.smoke.provider-class class-provider))

      ;; Attempting to remove a non-existent provider should not
      ;; run hooks.
      (setf (find-provider :mock 'bar :if-does-not-exist nil) nil)
      (check-call (calls 4)
        (:provider-removed hooks.smoke.provider-class class-provider)))))
