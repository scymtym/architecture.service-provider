;;;; architecture.service-provider.asd --- System definition of architecture.service-provider system.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem :architecture.service-provider
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version-string.sexp")
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
                              (:file       "variables")
                              (:file       "conditions")
                              (:file       "protocol")
                              (:file       "mixins")
                              (:file       "service")
                              (:file       "provider")
                              (:file       "macros")
                              (:file       "compilation"))))
  :in-order-to ((test-op (test-op :architecture.service-provider/test))))

(defsystem :architecture.service-provider/test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version-string.sexp")
  :license     "LLGPLv3" ; see COPYING file for details.
  :description "Unit tests of the service-provider system."
  :depends-on  (:alexandria
                (:version :let-plus                      "0.2")
                :more-conditions

                (:version :fiveam                        "1.3")

                (:version :architecture.service-provider (:read-file-form "version-string.sexp")))
  :components  ((:module     "test"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")
                              #+later (:file       "mixins")
                              (:file       "service")
                              (:file       "provider")
                              (:file       "macros")
                              (:file       "compilation")))))

(defmethod perform ((op        test-op)
                    (component (eql (find-system :architecture.service-provider/test))))
  (funcall (read-from-string "service-provider.test:run-tests")))
