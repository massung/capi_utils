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

(defclass record-layout (column-layout)
  ((selected-panes :initform nil :reader record-layout-selected-panes))
  (:default-initargs
   :vertical-scroll t))

(defmethod record-layout-select-pane ((layout record-layout) pane &key single-selection)
  "Add or remove the pane from the layout selection."
  (with-slots (selected-panes)
      layout
    
    ;; change the current selection
    (setf (record-layout-selected-panes layout)
          (cond (single-selection
                 (list pane))
            
                ;; if already selected, remove it from the selection
                ((record-layout-pane-selected-p layout pane)
                 (remove pane selected-panes))
          
                ;; otherwise add it to the selection
                (t (cons pane selected-panes))))))

(defmethod record-layout-select-pane-range ((layout record-layout) pane)
  "Change the selection to a range of panes."
  (if-let (cur (first (record-layout-selected-panes layout)))
      (let ((m (position cur (layout-description layout)))
            (n (position pane (layout-description layout))))
        (setf (record-layout-selected-panes layout)
              (subseq (layout-description layout) (min m n) (1+ (max m n)))))
    (record-layout-select-pane layout pane :single-selection t)))

(defmethod record-layout-select-all-panes ((layout record-layout))
  "Change the selection to all panes."
  (setf (record-layout-selected-panes layout) (layout-description layout)))

(defmethod record-layout-deselect-all-panes ((layout record-layout))
  "Clear the selection and redraw it."
  (setf (record-layout-selected-panes layout) nil))

(defmethod record-layout-pane-selected-p ((layout record-layout) pane)
  "T if the pane is in the layout and selected."
  (with-slots (selected-panes)
      layout
    (and (find pane selected-panes) (find pane (layout-description layout)))))

(defmethod (setf record-layout-selected-panes) (panes (layout record-layout))
  "Change the current selection."
  (with-slots (selected-panes)
      layout

    ;; remove panes not in the layout
    (flet ((pane-in-layout-p (pane)
             (find pane (layout-description layout))))
      (setf panes (remove-if-not #'pane-in-layout-p panes)))

    ;; deselect any panes not in the new selection, select panes that are new
    (let ((altered-panes (loop with single-select-p = (null (second panes))

                               ;; loop over all panes in the layout
                               for pane in (layout-description layout)

                               ;; is the pane being selected or is it already selected?
                               for selected = (find pane selected-panes)
                               for selecting = (find pane panes)

                               ;; select or deselect the pane
                               do (cond ((and (null selected) selecting)
                                             (record-pane-select-pane pane single-select-p))
                                            ((and (null selecting) selected)
                                             (record-pane-deselect-pane pane)))

                               ;; 
                               unless (eq selected selecting)
                               collect pane)))
      
      ;; set the current selection and redraw
      (setf selected-panes panes)
      
      ;; redraw altered panes
      (mapc #'gp:invalidate-rectangle altered-panes))))

(defmethod (setf layout-description) (panes (layout record-layout))
  "Remove selected items that aren't in the new description."
  (with-slots (selected-panes)
      layout
    (setf selected-panes (remove-if-not #'(lambda (pane) (find pane panes)) selected-panes))

    ;; continue to set the description
    (call-next-method panes layout)))
