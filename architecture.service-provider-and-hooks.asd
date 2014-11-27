;;;; architecture.service-provider-and-hooks.asd --- System definition for architecture.service-provider-and-hooks system.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

#.(progn
    (load (make-pathname :name     "architecture.service-provider"
                         :defaults *load-truename*))
    (values))

(cl:in-package #:architecture.service-provider-system)

;;; System definitions

(defsystem :architecture.service-provider-and-hooks
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3" ; see COPYING file for details.
  :description "Run hooks when service providers change."
  :depends-on  (:cl-hooks

                (:version :architecture.service-provider #.(version/string)))
  :components  ((:file       "hooks"
                 :pathname   "src/hooks"))
  :in-order-to ((test-op (test-op :architecture.service-provider-and-hooks-test))))

(defsystem :architecture.service-provider-and-hooks-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3" ; see COPYING file for details.
  :description "Unit tests of the architecture.service-provider-and-hooks system."
  :depends-on  (:alexandria

                (:version :fiveam                                  "1.1")

                (:version :architecture.service-provider-and-hooks #.(version/string))

                (:version :architecture.service-provider-test      #.(version/string)))
  :components  ((:file       "hooks"
                 :pathname   "test/hooks")))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :architecture.service-provider-and-hooks-test))))
  (eval (read-from-string "(5am:run! 'service-provider.test::service-provider.hooks)")))