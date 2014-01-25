;;;; threads.lisp --- Tests for concurrent (re)definition or services/providers.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:service-provider.test)

(in-suite service-provider)

(defclass define-service.concurrent.mock-service1 (standard-service) ())
(defclass define-service.concurrent.mock-service2 (standard-service) ())

(test define-service.concurrent

  (let+ (((&flet check-service ()
            (with-locked-services
              (let ((service (find-service 'foo)))
                (is (eq 'foo (service-name service)))
                (is-true (or (and )))))))
         (threads
          (loop
            :for i :to 16
            :collect (bt:make-thread
                      (cond
                        ((zerop i)
                         (lambda ()
                           (dotimes (j 1000)
                             ;; TODO Not yet supported in 5am.
                             #+no (check-service))))
                        ((oddp i)
                         (lambda ()
                           (dotimes (j 1000)
                             (define-service foo
                               (:service-class define-service.concurrent.mock-service1)
                               (:documentation "1")))))
                        (t
                         (lambda ()
                           (dotimes (j 1000)
                             (define-service foo
                               (:service-class define-service.concurrent.mock-service2)
                               (:documentation "2"))))))
                      ))))
    (mapc #'bt:join-thread threads)
    (check-service)))
