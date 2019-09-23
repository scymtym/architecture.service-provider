;;;; protocol.lisp --- Protocol provided by the architecture.service-provider system.
;;;;
;;;; Copyright (C) 2012-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:service-provider)

;;; Service protocol

(defgeneric service-name (service)
  (:documentation
   "Return the symbol which is the name of SERVICE."))

(defgeneric service-providers (service)
  (:documentation
   "Return a sequence of the providers of SERVICE."))

(defgeneric service-providers/alist (service)
  (:documentation
   "Return the providers of SERVICE as an alist in which CARs are
    provider names and CDRs are the corresponding provider objects."))

(defgeneric service-providers/plist (service)
  (:documentation
   "Return the providers of SERVICE as a plist in which keys are
    provider names and values are the corresponding provider
    objects."))

(defgeneric find-service (name
                          &key
                          if-does-not-exist)
  (:documentation
   "Find and return the service designated by the `service-designator'
    NAME.

    IF-DOES-NOT-EXIST controls the behavior in case the designated
    service cannot be found:

      The values #'error and 'error cause a `missing-service-error' to
      be signaled.

      The values #'warn and 'warn cause a `missing-service-warning' to
      be signaled and nil to be returned.

      The value nil causes nil to be returned without any conditions
      being signaled.

    `retry' and `use-value' restarts are established around error
    signaling (if IF-DOES-NOT-EXIST mandates that)."))

(defgeneric (setf find-service) (new-value name
                                 &key
                                 if-does-not-exist)
  (:documentation
   "Set the service designated by the `service-designator' NAME to
    NEW-VALUE. When non-nil, NEW-VALUE has to implement the service
    protocol.

    If NAME already designates a service, the existing service object
    is replaced with NEW-VALUE.

    If NEW-VALUE is nil, an existing service designated by NAME is
    removed.

    IF-DOES-NOT-EXIST is accepted for parity with `find-service' and
    usually ignored. However, when NEW-VALUE is nil, IF-DOES-NOT-EXIST
    controls whether an error should be signaled in case the
    to-be-removed service does not exist."))

;; Default behavior

(macrolet ((define-lookup-method (name)
             `(defmethod ,name ((service symbol))
                (,name (find-service service)))))
  (define-lookup-method service-name)
  (define-lookup-method service-providers)
  (define-lookup-method service-providers/alist)
  (define-lookup-method service-providers/plist))

(defmethod find-service ((name symbol)
                         &key
                         (if-does-not-exist #'error))
  (let ((name name))
    (tagbody
     :start
       (return-from find-service
         (or (gethash name *services*)
             (error-behavior-restart-case
                 (if-does-not-exist
                  (missing-service-error
                   :designator name)
                  :warning-condition   missing-service-warning
                  :allow-other-values? t)
               (retry ()
                 :report (lambda (stream)
                           (format stream "~@<Retry finding the ~
                                           service designated by ~
                                           ~S.~@:>"
                                   name))
                 (go :start))
               (retry-with-name (new-name)
                 :report      (lambda (stream)
                                (format stream "~@<Specify a name ~
                                                with which the lookup ~
                                                should be ~
                                                retried.~@:>"))
                 :interactive (lambda ()
                                (format *query-io* "Enter service ~
                                                    designator (unevaluated): ")
                                (finish-output *query-io*)
                                (list (read *query-io*)))
                 (setf name new-name)
                 (go :start))
               (use-value (value)
                 :report      (lambda (stream)
                                (format stream "~@<Specify a value ~
                                                which should be used ~
                                                as the service ~
                                                designated by ~S.~@:>"
                                        name))
                 :interactive (lambda ()
                                (format *query-io* "Enter value ~
                                                    (evaluated): ")
                                (finish-output *query-io*)
                                (list (eval (read *query-io*))))
                 value)))))))

(defmethod (setf find-service) ((new-value t)
                                (name      symbol)
                                &key
                                if-does-not-exist)
  (declare (ignore if-does-not-exist))

  (setf (gethash name *services*) new-value))

(defmethod (setf find-service) ((new-value (eql nil))
                                (name      symbol)
                                &key
                                if-does-not-exist)
  (unless (gethash name *services*)
    (error-behavior-restart-case
        (if-does-not-exist
         (missing-service-error
          :designator name)
         :warning-condition missing-service-warning)))

  (remhash name *services*)
  new-value)

(defmethod documentation ((slotd symbol) (doc-type (eql 'service)))
  (when-let ((service (find-service slotd :if-does-not-exist nil)))
    (documentation service t)))

;;; Provider protocol

(defgeneric provider-name (provider)
  (:documentation
   "Return the symbol which is the name of PROVIDER."))

(defgeneric find-provider (service provider
                           &key
                           if-does-not-exist)
  (:documentation
   "Find and return the provider designated by the
    `provider-designator' PROVIDER in the service designated by the
    `service-designator' SERVICE.

    IF-DOES-NOT-EXIST controls the behavior in case SERVICE or
    PROVIDER cannot be found:

      The values #'error and 'error cause a `missing-service-error' to
      be signaled if SERVICE cannot be found and a
      `missing-provider-error' to be signaled if PROVIDER cannot be
      found.

      The values #'warn and 'warn cause a `missing-service-warning' to
      be signaled if SERVICE cannot be found and a
      `missing-provider-warning' to be signaled if PROVIDER cannot be
      found. In both cases, nil is returned.

      The value nil causes nil to be returned without any conditions
      being signaled.

    `retry' and `use-value' restarts are established around error
    signaling (if IF-DOES-NOT-EXIST mandates that)."))

(defgeneric (setf find-provider) (new-value service provider
                                  &key
                                  if-does-not-exist)
  (:documentation
   "Set the provider designated by the `provider-designator' PROVIDER
    in the service designated by the `service-designator' SERVICE to
    NEW-VALUE. When non-nil, NEW-VALUE has to implement the provider
    protocol.

    If SERVICE and PROVIDER already designate a provider, the existing
    provider object is replaced with NEW-VALUE.

    If NEW-VALUE is nil, an existing provider designated by SERVICE
    and PROVIDER is removed.

    IF-DOES-NOT-EXIST is accepted for parity with `find-provider' and
    usually ignored. However, when NEW-VALUE is nil, IF-DOES-NOT-EXIST
    controls whether an error should be signaled in case the
    to-be-removed provider does not exist."))

(defgeneric update-provider (service name provider)
  (:documentation
   "Update the provider designated by NAME in SERVICE with the new
    value PROVIDER."))

(defgeneric add-provider (service name provider)
  (:documentation
   "Add PROVIDER to SERVICE as the provider designated by NAME."))

(defgeneric remove-provider (service name provider)
  (:documentation
   "Remove PROVIDER from SERVICE as the provider designated by NAME."))

(defgeneric make-provider (service provider &rest args)
  (:documentation
   "Make and return an instance of the provider designated by the
    `provider-designator' PROVIDER of the service designated by the
    `service-designator' SERVICE."))

(defgeneric make-provider-form (service provider args environment)
  (:documentation
   "Return a form which makes and returns an instance of the provider
    designated by the `provider-designator' PROVIDER of the service
    designated by the `service-designator' SERVICE for ARGS and
    ENVIRONMENT."))

;; Default behavior

(defmethod find-provider :around ((service  t)
                                  (provider t)
                                  &key
                                  (if-does-not-exist #'error))
  (let ((provider provider))
    (tagbody
       ;; This avoids multiple warnings when IF-DOES-NOT-EXIST is
       ;; `warn'.
       (when (symbolp service)
         (return-from find-provider
           (call-next-method service provider
                             :if-does-not-exist if-does-not-exist)))
     :start
       (return-from find-provider
         (or (call-next-method service provider
                               :if-does-not-exist if-does-not-exist)
             (error-behavior-restart-case
                 (if-does-not-exist
                  (missing-provider-error
                   :service    service
                   :designator provider)
                  :warning-condition missing-provider-warning)
               (retry () ; TODO(jmoringe, 2012-09-03): who should establish the restart?
                 :report (lambda (stream)
                           (format stream "~@<Retry finding the ~
                                           provider of service ~A ~
                                           designated by ~S.~@:>"
                                   service provider))
                 (go :start))
               (retry-with-name (new-name)
                 :report      (lambda (stream)
                                (format stream "~@<Specify a name with ~
                                                which the lookup should ~
                                                be retried.~@:>"))
                 :interactive (lambda ()
                                (format *query-io* "Enter provider ~
                                                    designator (unevaluated): ")
                                (finish-output *query-io*)
                                (list (read *query-io*)))
                 (setf provider new-name)
                 (go :start))
               (use-value (value)
                 :report      (lambda (stream)
                                (format stream "~@<Specify a value ~
                                                which should be used ~
                                                as the provider of ~
                                                service ~A designated ~
                                                by ~S.~@:>"
                                        service provider))
                 :interactive (lambda ()
                                (format *query-io* "Enter ~
                                                    value (evaluated): ")
                                (finish-output *query-io*)
                                (list (eval (read *query-io*))))
                 value)))))))

(defmethod find-provider ((service  symbol)
                          (provider t)
                          &key
                          if-does-not-exist)
  (declare (notinline find-provider))
  (when-let ((service (find-service
                       service :if-does-not-exist if-does-not-exist)))
    (find-provider service provider :if-does-not-exist if-does-not-exist)))

(defmethod (setf find-provider) ((new-value t)
                                 (service   symbol)
                                 (provider  t)
                                 &key
                                 (if-does-not-exist #'warn))
  (declare (notinline (setf find-provider)))
  ;; IF-DOES-NOT-EXIST is only relevant in case NEW-VALUE is nil,
  ;; i.e. a provider is being removed.
  (setf (find-provider (find-service service) provider
                       :if-does-not-exist if-does-not-exist)
        new-value))

(defmethod make-provider ((service  symbol)
                          (provider t)
                          &rest args)
  (declare (notinline make-provider))
  (apply #'make-provider (find-service service) provider args))

(defmethod make-provider ((service  t)
                          (provider t)
                          &rest args)
  (declare (notinline make-provider find-provider))
  (apply #'make-provider service (find-provider service provider) args))

(defmethod make-provider-form ((service     symbol)
                               (provider    t)
                               (args        t)
                               (environment t))
  (make-provider-form (find-service service) provider args environment))

(defmethod make-provider-form ((service     t)
                               (provider    t)
                               (args        t)
                               (environment t))
  (declare (notinline find-provider))
  (make-provider-form service (find-provider service provider)
                      args environment))
