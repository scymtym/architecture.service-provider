;;;; architecture.service-provider.asd --- System definition of architecture.service-provider system.
;;;;
;;;; Copyright (C) 2012, 2013, 2014, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:architecture.service-provider-system
  (:use
   #:cl
   #:asdf)

  (:export
   #:version/list
   #:version/string))

(cl:in-package #:architecture.service-provider-system)

;;; Version stuff

(defparameter +version-major+ 0
  "Major component of version number.")

(defparameter +version-minor+ 1
  "Minor component of version number.")

(defparameter +version-revision+ 0
  "Revision component of version number.")

(defun version/list ()
  "Return a version of the form (MAJOR MINOR REVISION)."
  (list +version-major+ +version-minor+ +version-revision+))

(defun version/string ()
  "Return a version string of the form \"MAJOR.MINOR.REVISION\"."
  (format nil "~{~A.~A.~A~}" (version/list)))

;;; System definitions

(defsystem :architecture.service-provider
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3" ; see COPYING file for details.
  :description "Provides a framework for registering and finding services and providers of these."
  :depends-on  (:alexandria
                (:version :let-plus              "0.2")
                (:version :more-conditions       "0.3.0")
                (:version :utilities.print-items "0.1.0"))
  :components  ((:module     "src"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "types")
                              (:file       "conditions")
                              (:file       "protocol")
                              (:file       "mixins")
                              (:file       "service")
                              (:file       "provider")
                              (:file       "macros")
                              #+later (:file       "compilation"))))
  :in-order-to ((test-op (test-op :architecture.service-provider-test))))

(defsystem :architecture.service-provider-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3" ; see COPYING file for details.
  :description "Unit tests of the service-provider system."
  :depends-on  (:alexandria
                (:version :let-plus                      "0.2")
                :more-conditions

                (:version :fiveam                        "1.1")

                (:version :architecture.service-provider #.(version/string)))
  :components  ((:module     "test"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")
                              #+later (:file       "mixins")
                              (:file       "service")
                              (:file       "provider")
                              (:file       "macros")
                              #+later (:file       "compilation")))))

(defmethod perform ((op        test-op)
                    (component (eql (find-system :architecture.service-provider-test))))
  (funcall (read-from-string "service-provider.test:run-tests")))
