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

(defclass record-pane (output-pane)
  ((action-callback  :initform nil :initarg :action-callback   :accessor record-pane-action-callback)
   (select-callback  :initform nil :initarg :selected-callback :accessor record-pane-selected-callback)
   (retract-callback :initform nil :initarg :retract-callback  :accessor record-pane-retract-callback)
   (drag-callback    :initform nil :initarg :drag-callback     :accessor record-pane-drag-callback)
   (filter-callback  :initform nil :initarg :filter-callback   :accessor record-pane-filter-callback)
   (menu             :initform nil :initarg :alt-action-menu   :accessor record-pane-alt-action-menu))
  (:default-initargs
   :input-model '(((:button-1 :press) click-record-pane)
                  ((:button-1 :second-press) double-click-record-pane)
                  ((:button-1 :press :shift) shift-click-record-pane)
                  ((:button-1 :press #+cocoa :hyper #+mswindows :control) hyper-click-record-pane)
                  ((:motion :button-1 :press) drag-record-pane)
                  (:post-menu post-menu-record-pane))))

(defmethod record-pane-filter ((pane record-pane) text)
  "T if this pane should be filtered."
  (if-let (callback (record-pane-selected-callback pane))
      (funcall callback pane text)
    t))

(defmethod record-pane-selected-p ((pane record-pane))
  "T if the pane is currently selected."
  (record-layout-pane-selected-p (element-parent pane) pane))

(defmethod record-pane-select-pane ((pane record-pane) single-select-p)
  "Called by the record layout."
  (when-let (callback (record-pane-selected-callback pane))
    (funcall callback pane single-select-p)))

(defmethod record-pane-deselect-pane ((pane record-pane))
  "Called by the record layout."
  (when-let (callback (record-pane-retract-callback pane))
    (funcall callback pane)))

(defmethod click-record-pane ((pane record-pane) x y)
  "Select the record."
  (record-layout-select-pane (element-parent pane) pane :single-selection t))

(defmethod double-click-record-pane ((pane record-pane) x y)
  "Perform the action on this record."
  (when-let (callback (record-pane-action-callback pane))
    (funcall callback pane)))

(defmethod shift-click-record-pane ((pane record-pane) x y)
  "Select a range of records."
  (record-layout-select-pane-range (element-parent pane) pane))

(defmethod hyper-click-record-pane ((pane record-pane) x y)
  "Toggle this record as part of the selection."
  (record-layout-select-pane (element-parent pane) pane))

(defmethod drag-record-pane ((pane record-pane) x y)
  "Begin a drag operation."
  (when-let (callback (record-pane-drag-callback pane))
    (funcall callback pane x y)))

(defmethod post-menu-record-pane ((pane record-pane) x y &optional gspec)
  "Right-click action on this record."
  (unless (record-pane-selected-p pane)
    (record-layout-select-pane (element-parent pane) pane :single-selection t))

  ;; show the menu
  (when-let (menu (record-pane-alt-action-menu pane))
    (display-popup-menu (funcall menu (top-level-interface pane)) :owner pane :x x :y y)))
