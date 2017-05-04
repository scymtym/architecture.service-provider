;;;; variables.lisp --- Variables provided by the architecture.service-provider system.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:service-provider)

(defvar *services* (make-hash-table)
  "Stores a mapping of service names to service objects.")
