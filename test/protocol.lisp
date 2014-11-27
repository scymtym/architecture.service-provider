;;;; protocol.lisp --- Unit tests for the protocol functions of the architecture.service-provider system.
;;;;
;;;; Copyright (C) 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:service-provider.test)

(in-suite service-provider)

;;; Service accessor tests

(test protocol.service-accessors.designators
  "Check that service accessors accept designators."

  ;; When the service is defined, all accessors should work on its
  ;; designator.
  (with-service (:mock (:documentation "foo"))
    (is (eq    :mock (service-name :mock)))
    (is (equal '()   (service-providers :mock)))
    (is (equal "foo" (documentation :mock 'service-provider::service)))
    (is (equal '()   (service-providers/alist :mock)))
    (is (equal '()   (service-providers/plist :mock))))

  ;; For an undefined service, all accessors should signal the usual
  ;; `missing-service-error'.
  (macrolet ((test-case (form)
               `(signals missing-service-error ,form)))

    (test-case (service-name :no-such-service))
    (test-case (service-providers :no-such-service))
    (test-case (service-providers/alist :no-such-service))
    (test-case (service-providers/plist :no-such-service)))

  (is (null (documentation :no-such-service 'service-provider::service))))

(test protocol.service-providers.smoke
  "Smoke test for the service-providers{,/alist,/plist} functions."

  (with-service (:mock1)
    (register-provider/function :mock1 :mock2 :function 'list)
    (let ((provider (find-provider :mock1 :mock2)))

      (is (equal (list provider)
                 (service-providers :mock1)))
      (is (equal (list (cons :mock2 provider))
                 (service-providers/alist :mock1)))
      (is (equal (list :mock2 provider)
                 (service-providers/plist :mock1))))))

;;; `find-service' tests

(test protocol.find-service.conditions
  "Check conditions signaled by the `find-service' generic function."

  ;; A `missing-service-error' should be signaled when a service
  ;; cannot be found.
  (signals missing-service-error (find-service 'no-such-service))

  ;; Request a warning instead of an error.
  (signals missing-service-warning
    (find-service 'no-such-service :if-does-not-exist #'warn))

  ;; Request nil instead of error.
  (is (null (find-service 'no-such-service :if-does-not-exist nil))))

(test protocol.find-service.restarts
  "Check restarts established by the `find-service' generic function."

  (macrolet
      ((with-restart-fixture ((service) &body body)
         `(with-cleaned-up-service (,service)
            (handler-bind
                ((missing-service-error (lambda (condition)
                                          (declare (ignorable condition))
                                          ,@body)))
              (find-service ',service)))))

    (let ((service (make-instance 'standard-service
                                  :name 'no-such-service)))
      ;; Create the missing service and retry.
      (is (eq service
              (with-restart-fixture (no-such-service)
                (let ((restart (find-restart 'retry condition)))
                  (is (typep restart 'restart))
                  (does-not-signal error (princ-to-string restart))
                  (setf (find-service 'no-such-service) service)
                  (invoke-restart restart)))))

      ;; Use an arbitrary value instead of the missing service.
      (is (eq service
              (with-restart-fixture (no-such-service)
                (let ((restart (find-restart 'use-value condition)))
                  (is (typep restart 'restart))
                  (does-not-signal error (princ-to-string restart))
                  (invoke-restart restart service))))))))

;;; `find-provider' tests

(test protocol.find-provider.smoke.type-error
  "Test signaling of `type-error' when `find-provider' is called with
   something other than a `provider-designator'."

  (with-service (:mock)
    (signals type-error (find-provider :mock nil))
    (signals type-error (find-provider :mock (cons 1 nil)))))

(test protocol.find-provider.smoke.symbol-designator
  "Check `find-provider' works with symbols as
   `provider-designator's."

  (let ((designator :list))
    (with-service (:mock)
      (register-provider/function :mock designator :function 'list)
      (is-true (find-provider :mock designator))
      (is (eq designator (provider-name
                          (find-provider :mock designator)))))))

(test protocol.find-provider.smoke.non-symbol-designator
  "Check `find-provider' works with lists as `provider-designator's."

  (let ((designator '(:list real)))
    (with-service (:mock)
      (register-provider/function :mock designator :function 'list)
      (is-true (find-provider :mock designator))
      (is (equal designator (provider-name
                             (find-provider :mock designator)))))))

(test protocol.find-provider.conditions
  "Check conditions signaled by the `find-provider' generic function."

  ;; When the service cannot be found, the provider name does not
  ;; matter. Otherwise a `missing-provider-error' should be signaled.
  (signals missing-service-error
    (find-provider 'no-such-service 'does-not-matter))
  (with-service (:mock)
    (signals missing-provider-error
      (find-provider :mock 'no-such-provider)))

  ;; Same behavior as before but with warnings instead of errors.
  (signals missing-service-warning
    (find-provider 'no-such-service 'does-not-matter
                   :if-does-not-exist #'warn))
  (with-service (:mock)
    (signals missing-provider-warning
      (find-provider :mock 'no-such-provider
                     :if-does-not-exist #'warn)))

  ;; Request returning nil in case services or providers are not
  ;; found.
  (is (null (find-provider 'no-such-service 'does-not-matter
                           :if-does-not-exist nil)))
  (with-service (:mock)
    (is (null (find-provider :mock 'no-such-provider
                             :if-does-not-exist nil)))))

(test protocol.find-provider.restarts
  "Check restarts established by the `find-provider' generic function."

  (with-service (:mock)
    (macrolet
        ((with-restart-fixture ((provider) &body body)
           `(handler-bind
                ((missing-provider-error (lambda (condition)
                                           (declare (ignorable condition))
                                           ,@body)))
              (find-provider :mock ',provider))))

      (let ((provider (make-instance 'function-provider
                                     :name     'no-such-provider
                                     :function 'car)))
        ;; Register the missing provider and retry.
        (is (eq provider
                (with-restart-fixture (no-such-provider)
                  (let ((restart (find-restart 'retry condition)))
                    (is (typep restart 'restart))
                    (does-not-signal error (princ-to-string restart))
                    (setf (find-provider :mock 'no-such-provider) provider)
                    (invoke-restart restart)))))

        ;; Use an arbitrary value instead of the missing service.
        (is (eq provider
                (with-restart-fixture (no-such-provider)
                  (let ((restart (find-restart 'use-value condition)))
                    (is (typep restart 'restart))
                    (does-not-signal error (princ-to-string restart))
                    (invoke-restart 'restart provider)))))))))

(test protocol.find-provider.undefinition
  "Test undefining providers via (setf (find-provider ...) nil)."

  (macrolet
      ((test-case (&body body)
         `(with-service (:mock) ,@body))
       (test-case/with-mock-provider (&body body)
         `(test-case
           (register-provider/function :mock :mock2 :function 'list)
           ,@body)))

    ;; When an attempt is made to remove a non-existent provider, a
    ;; warning should maybe be signaled depending on the error policy.
    (test-case
     (signals missing-provider-warning
       (setf (find-provider :mock :does-not-exist) nil)))
    (test-case
     (does-not-signal missing-provider-warning
       (setf (find-provider :mock :does-not-exist :if-does-not-exist nil) nil)))

    ;; When an existing provider is undefined, no condition should be
    ;; signaled whatsoever and the provider should be gone afterward.
    (test-case/with-mock-provider
     (find-provider :mock :mock2) ; sanity check: provider should be there
     (does-not-signal missing-provider-warning
       (setf (find-provider :mock :mock2) nil))
     (is (null (find-provider :mock :mock2 :if-does-not-exist nil))))
    (test-case/with-mock-provider
     (find-provider :mock :mock2) ; sanity check: provider should be there
     (does-not-signal missing-provider-warning
       (setf (find-provider :mock :mock2 :if-does-not-exist nil) nil))
     (is (null (find-provider :mock :mock2 :if-does-not-exist nil))))))
