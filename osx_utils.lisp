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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(center-interface

            ;; user functions
            get-user-name

            ;; application functions
            open-file-with-application

            ;; app menu actions
            hide-application
            hide-other-applications
            unhide-application
            unhide-all-applications)))

(define-foreign-function (ns-full-user-name "NSFullUserName")
    ()
  :result-type objc:objc-object-pointer)

(defun center-interface (interface)
  "Position a window in the center of the screen."
  (when (capi-internals:representation interface)
    (let ((window (objc:invoke (cocoa-view-pane-view interface) "window")))
      (objc:invoke window "center"))))

(defun get-user-name ()
  "Returns the current user's name."
  (convert-from-foreign-string (objc:invoke (ns-full-user-name) "UTF8String")))

(defun open-file-with-application (pathname &optional app)
  "Launch an application to open a file."
  (let ((workspace (objc:invoke "NSWorkspace" "sharedWorkspace")))
    (if app
        (objc:invoke workspace "openFile:withApplication:" (namestring pathname) app)
      (objc:invoke workspace "openFile:" (namestring pathname)))))

(defun hide-application ()
  "Hide the current application."
  (let ((app (objc:invoke "NSApplication" "sharedApplication")))
    (objc:invoke app "hide:" nil)))

(defun hide-other-applications ()
  "Hide other running applications besides this one."
  (let ((app (objc:invoke "NSApplication" "sharedApplication")))
    (objc:invoke app "hideOtherApplications:" nil)))

(defun unhide-application ()
  "Show the current application."
  (let ((app (objc:invoke "NSApplication" "sharedApplication")))
    (objc:invoke app "unhide:" nil)))

(defun unhide-all-applications ()
  "Show all applications."
  (let ((app (objc:invoke "NSApplication" "sharedApplication")))
    (objc:invoke app "unhideAllApplications:" nil)))