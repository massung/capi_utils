;;;; CAPI Utility Panes for LispWorks
;;;;
;;;; Copyright (c) 2014 by Jeffrey Massung
;;;;
;;;; This file is provided to you under the Apache License,
;;;; Version 2.0 (the "License"); you may not use this file
;;;; except in compliance with the License. You may obtain
;;;; a copy of the License at
;;;;
;;;; http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing,
;;;; software distributed under the License is distributed on an
;;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;;; KIND, either express or implied. See the License for the
;;;; specific language governing permissions and limitations
;;;; under the License.
;;;;

(in-package :capi-utils)

(eval-when (:load-toplevel :execute)
  (register-module :secur32 :real-name "secur32.dll" :connection-style :immediate)

  ;; export symbols into capi-utils
  (export '(get-user-name)))

(define-foreign-function (get-user-name-ex "GetUserNameExW")
    ((format :int)
     (buffer (:pointer :byte))
     (size (:pointer :unsigned-long)))
  :result-type :boolean
  :module :secur32)

(defun get-user-name ()
  "Return the current user's name."
  (with-dynamic-foreign-objects ((buf :byte :fill 0 :nelems 1000)
                                 (size :unsigned-long :initial-element 1000))
    (when (get-user-name-ex 3 buf size)
      (convert-from-foreign-string buf :external-format :unicode :length (dereference size)))))