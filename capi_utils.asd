(defpackage :capi-utils-asd
  (:use :cl :asdf))

(in-package :capi-utils-asd)

(defsystem :capi_utils
  :name "capi_utils"
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "CAPI Utility Panes for LispWorks."
  :serial t
  :components ((:file "capi_utils")
               (:file "search_pane")
               (:file "record_layout")
               (:file "record_pane")))
