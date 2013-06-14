;;;; protocol.lisp --- Unit tests for the protocol functions of the architecture.service-provider system.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:service-provider.test)

(in-suite service-provider)

;;; `find-service' tests

(test protocol.find-service.conditions

  ;; TODO
  (signals missing-service-error (find-service 'no-such-service))

  ;; TODO
  (signals missing-service-warning
    (find-service 'no-such-service :if-does-not-exist #'warn))

  ;; TODO
  (is (null (find-service 'no-such-service :if-does-not-exist nil))))

(test protocol.find-service.restarts

  (macrolet
      ((with-restart-fixture ((service) &body body)
         `(with-cleaned-up-service (,service)
            (handler-bind
                ((error (lambda (condition)
                          (declare (ignorable condition))
                          ,@body)))
              (find-service ',service)))))

    ;; TODO
    (let ((service (make-instance 'service-provider::standard-service
                                  :name 'no-such-service)))
      (is (eq (with-restart-fixture (no-such-service)
                (is (find-restart 'retry condition))
                (setf (find-service 'no-such-service) service)
                (invoke-restart 'retry))
              service)))

    ;; TODO
    (is (eq (with-restart-fixture (no-such-service)
              (is (find-restart 'use-value condition))
              (invoke-restart 'use-value :foo))
            :foo))))
