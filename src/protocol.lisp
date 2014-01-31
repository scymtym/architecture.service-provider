;;;; protocol.lisp --- Protocol provided by the architecture.service-provider system.
;;;;
;;;; Copyright (C) 2012, 2013, 2014 Jan Moringen
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
    signaling (if IF-DOES-NOT-EXIST mandates that).

    TODO There is a compiler-macro for this function which tries to
    find the designated service at compile-time and load-time if the
    designator is constant. Compile- and load-time `style-warning' are
    signaled if a constant designator cannot be found."))

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

;;; TODO(jmoringe, 2012-12-16): move to suitable file

(defvar *services* (make-hash-table)
  "Stores a mapping of service names to service objects.")

(defmethod find-service ((name symbol)
                         &key
                         (if-does-not-exist #'error))
  (labels
      ((recur ()
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
                 (recur))
               (use-value (value)
                 :report (lambda (stream)
                           (format stream "~@<Specify a value which ~
                                           should be used as the ~
                                           service designated by ~
                                           ~S.~@:>"
                                   name))
                 value)))))
    (recur)))

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
    signaling (if IF-DOES-NOT-EXIST mandates that).

    TODO There is a compiler-macro for this function which tries to
    find the designated service and provider at compile-time and
    load-time if the designators are constant. Compile- and load-time
    `style-warning's are signaled if a constant designator cannot be
    found."))

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
  (labels
      ((recur ()
         (or (call-next-method service provider
                               :if-does-not-exist if-does-not-exist)
             #+later (error-behavior-restart-case
                (if-does-not-exist
                 (missing-service-error
                  :service    service
                  :designator provider)
                 :warning-condition missing-service-warning)
              (retry ()
                (recur))
              (use-value (value)
                value))
             (etypecase if-does-not-exist
               (null
                nil)
               (function
                (restart-case
                    (funcall if-does-not-exist
                             (make-condition
                              (cond
                                ((member if-does-not-exist `(warn ,#'warn))
                                 'missing-provider-warning)
                                (t
                                 'missing-provider-error))
                              :service    service
                              :designator provider))
                  (retry () ;;; TODO(jmoringe, 2012-09-03): who should establish the restart?
                    :report (lambda (stream)
                              (format stream "~@<Retry finding the ~
                                                 provider of service ~
                                                 ~A designated by ~
                                                 ~S.~@:>"
                                      service provider))
                    (recur))
                  (use-value (value)
                    :report (lambda (stream)
                              (format stream "~@<Specify a value which ~
                                                 should be used as the ~
                                                 provider of service ~
                                                 ~A designated by ~S.~@:>"
                                      service provider))
                    value)))))))
    ;; This avoids multiple warnings when IF-DOES-NOT-EXIST is `warn'.
    (if (symbolp service)
        (call-next-method service provider
                          :if-does-not-exist if-does-not-exist)
        (recur))))

(defmethod find-provider ((service  symbol)
                          (provider t)
                          &key
                          if-does-not-exist)
  (when-let ((service (find-service
                       service :if-does-not-exist if-does-not-exist)))
    (find-provider service provider :if-does-not-exist if-does-not-exist)))

(defmethod (setf find-provider) ((new-value t)
                                 (service   symbol)
                                 (provider  t)
                                 &key
                                 (if-does-not-exist #'warn))
  (setf (find-provider (find-service service) provider
                       :if-does-not-exist if-does-not-exist)
        new-value))

(defmethod make-provider ((service  symbol)
                          (provider t)
                          &rest args)
  (apply #'make-provider (find-service service) provider args))

(defmethod make-provider ((service  t)
                          (provider t)
                          &rest args)
  (apply #'make-provider service (find-provider service provider) args))

;;; TODO(jmoringe, 2012-12-23): avoid redundancy

(defmethod make-provider-form ((service     symbol)
                               (provider    t)
                               (args        t)
                               (environment t))
  (make-provider-form (find-service service) provider args environment))

(defmethod make-provider-form ((service     t)
                               (provider    t)
                               (args        t)
                               (environment t))
  (make-provider-form service (find-provider service provider)
                      args environment))
