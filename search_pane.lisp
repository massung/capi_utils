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

(defparameter *default-search-pane-font*
  #+cocoa (gp:make-font-description :family "Helvetica Neue")
  #+mswindows (gp:make-font-description :stock :system-font :size 8))

(defclass search-text-pane (text-input-pane)
  ((last-search     :initform nil :initarg :last-search     :accessor search-text-pane-last-search)
   (live-search-p   :initform t   :initarg :live-search-p   :accessor search-text-pane-live-search-p)
   (search-callback :initform nil :initarg :search-callback :accessor search-text-pane-search-callback))
  (:default-initargs
   :search-field "Search"
   :maximum-recent-items 0
   :recent-items-mode :delayed
   :accepts-focus-p t
   :font *default-search-pane-font*
   :text-change-callback 'update-search-text-pane
   :callback 'perform-search))

(defmethod update-search-text-pane (text (pane search-text-pane) interface pos)
  "The search text was modified, call the search callback."
  (when (search-text-pane-live-search-p pane)
    (search-text-pane-perform-search pane :return)))

(defmethod search-text-pane-perform-search ((pane search-text-pane) (gesture (eql :return)))
  "Execute a new search."
  (when-let (callback (search-text-pane-search-callback pane))
    (funcall callback pane (text-input-pane-text pane))))

#+cocoa
(defmethod (setf search-text-pane-placeholder-text) (text (pane search-text-pane))
  "Allow a string to be visisble in the text pane when not in focus."
  (let ((view (cocoa-view-pane-view pane)))
    (objc:invoke (objc:invoke view "cell") "setPlaceholderString:" text)))

#+mswindows
(defmethod (setf search-text-pane-placeholder-text) (text (pane search-text-pane))
  "Allow a string to be visisble in the text pane when not in focus."
  (let ((hwnd (simple-pane-handle pane))
        (external-format (if (string= (software-type) "Windows NT") :unicode :ascii)))
    (unless (or (null hwnd) (zerop hwnd))
      (with-foreign-string (new-ptr n bytes :external-format external-format)
          text
        (declare (ignore n bytes))
        (win32:send-message hwnd #x1501 0 new-ptr)))))