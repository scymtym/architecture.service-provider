;;;; compilation.lisp --- Compile-time optimizations/checks of make-provider calls.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:service-provider)

(defun check-service-and-provider (service-form
                                   &optional
                                   (provider-form nil provider-form-supplied?))
  (declare (notinline find-service find-provider))
  (let+ (((&flet make-value-function (form form-provided? checkable-type)
            (let (value value?)
              (lambda ()
                (cond
                  ((or (not form-provided?) (not (constantp form)) value?)
                   (setf value? t)
                   value)
                  (t
                   (let ((value1 (eval form)))
                     (setf value? t)
                     (when (typep value1 checkable-type)
                       (setf value value1)))))))))
         (service-value  (make-value-function service-form t
                                              '(and symbol (not null))))
         (provider-value (make-value-function
                          provider-form provider-form-supplied?
                          '(or provider-designator/symbol
                               provider-designator/cons))))
    (cond
      ;; SERVICE is not suitable for checking.
      ((not (funcall service-value)))

      ;; SERVICE is suitable, but PROVIDER is not.
      ((not (funcall provider-value))
       (find-service (funcall service-value) :if-does-not-exist #'warn))

      ;; SERVICE and PROVIDER are suitable.
      (t
       (find-provider (funcall service-value) (funcall provider-value)
                      :if-does-not-exist #'warn)))))

;;; Compile-time checks for service existence

(macrolet ((define (name lambda-list
                    &key
                    (service-parameter (first lambda-list))
                    provider-parameter)
             (let+ (((&values required optional &ign key)
                     (parse-ordinary-lambda-list lambda-list))
                    (other-parameters (set-difference
                                       (append required
                                               (mapcar #'first optional)
                                               (mapcar #'cadar key))
                                       (list service-parameter
                                             provider-parameter))))
               `(define-compiler-macro ,name (&whole form ,@lambda-list)
                  (declare (ignore ,@other-parameters))
                  (check-service-and-provider
                   ,service-parameter
                   ,@(when provider-parameter `(,provider-parameter)))
                  form))))
  (define find-provider (service provider &key if-does-not-exist))
  (define (setf find-provider) (new-value service provider
                                          &key if-does-not-exist)
    :service-parameter service)
  (define update-provider (service name provider))
  (define add-provider (service name provider))
  (define remove-provider (service name provider))
  (define register-provider (service-name provider-name
                             provider-class initargs))
  (define register-provider/class (service-name provider-name
                                   &key &allow-other-keys))
  (define register-provider/function (service-name provider-name
                                      &key &allow-other-keys)))

;;; Compile-time checks for service and provider existence

(define-compiler-macro make-provider (&whole form service provider &rest args)
  (declare (ignore args))
  (check-service-and-provider service provider)
  form)
