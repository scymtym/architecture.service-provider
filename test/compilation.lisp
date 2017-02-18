;;;; compilation.lisp --- Unit tests for compile-time optimizations.
;;;;
;;;; Copyright (C) 2013, 2014, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:service-provider.test)

(in-suite service-provider)

;;; `find-service', `update-provider', etc.
;;;
;;; These are expected to signal a compile-time style-warning if the
;;; specified service does not exist.

(macrolet
    ((define (name call)
       (let ((test-name (symbolicate '#:compilation. name '#:.conditions)))
         `(test ,test-name

            ,(format nil "Test compile-time warnings for `~(~A~)' ~
                          calls."
                     name)
            (compilation-signals missing-service-warning ()
              ,(sublis '((&service . 'no-such-service)) call))
            (compilation-does-not-signal missing-service-warning (service)
              ,(sublis '((&service . service)) call))))))

  (define find-provider
      (find-provider &service :provider))

  (define setf-find-provider
      (setf (find-provider &service :provider) nil))

  (define update-provider
      (update-provider &service :provider nil))

  (define add-provider
      (add-provider &service :provider nil))

  (define remove-provider
      (remove-provider &service :provider nil))

  (define register-provider
      (register-provider &service :provider 'provider-class '()))

  (define register-provider/class
      (register-provider/class &service :provider))

  (define register-provider/function
      (register-provider/function &service :provider)))

;;; `make-provider'
;;;
;;; This is expected to signal compile-time style-warnings if the
;;; specified service or the specified provider does not exist.

(test compilation.make-provider.smoke
  "Make sure the compiler-macro on `make-provider' does not affect the
   runtime behavior."

  (signals missing-service-error
    (ignoring-compile-time-warnings (missing-service-warning)
      (make-provider 'no-such-service :no-such-provider)))

  (with-service (:mock)
    (signals missing-provider-error
      (ignoring-compile-time-warnings (missing-provider-warning)
        (make-provider :mock :no-such-provider)))))

(test compilation.make-provider.conditions
  "Test compile-time warnings for `make-provider' calls."

  (compilation-signals missing-service-warning ()
    (make-provider 'no-such-service :no-such-provider))

  (with-service (:mock)
    (compilation-signals missing-provider-warning ()
      (make-provider :mock :no-such-provider))))
