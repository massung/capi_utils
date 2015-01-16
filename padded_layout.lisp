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

(defclass padded-layout (grid-layout)
  ((inner-pane    :initarg :inner-pane    :accessor padded-layout-inner-pane    :initform nil)
   (top-margin    :initarg :top-margin    :accessor padded-layout-top-margin    :initform nil)
   (right-margin  :initarg :right-margin  :accessor padded-layout-right-margin  :initform nil)
   (bottom-margin :initarg :bottom-margin :accessor padded-layout-bottom-margin :initform nil)
   (left-margin   :initarg :left-margin   :accessor padded-layout-left-margin   :initform nil))
  (:extra-initargs '(:margin))
  (:default-initargs
   :rows 3
   :columns 3
   :x-adjust :center
   :y-adjust :center
   :x-ratios '(nil 1 nil)
   :y-ratios '(nil 1 nil)))

(defclass x-margin-pane (output-pane)
  ()
  (:default-initargs
   :visible-border nil
   :visible-max-width t
   :visible-min-height 0
   :visible-max-height 0))

(defclass y-margin-pane (output-pane)
  ()
  (:default-initargs
   :visible-border nil
   :visible-min-width 0
   :visible-max-width 0
   :visible-max-height t))

(defmethod initialize-instance :after ((layout padded-layout) &key margin)
  "Create all the initial panes for the padded layout."
  (with-slots (inner-pane top-margin right-margin bottom-margin left-margin)
      layout
    
    ;; if :margin was passed, make the margin universal
    (setf top-margin (or top-margin margin 0)
          right-margin (or right-margin margin 0)
          bottom-margin (or bottom-margin margin 0)
          left-margin (or left-margin margin 0))
    
    ;; create the panes for the layout
    (setf (layout-description layout)
          (let ((top-pane (make-instance 'y-margin-pane :visible-min-height top-margin))
                (right-pane (make-instance 'x-margin-pane :visible-min-width right-margin))
                (bottom-pane (make-instance 'y-margin-pane :visible-min-height bottom-margin))
                (left-pane (make-instance 'x-margin-pane :visible-min-width left-margin)))
            (list nil top-pane nil left-pane inner-pane right-pane nil bottom-pane nil)))))

(defmethod (setf padded-layout-inner-pane) :after (pane (layout padded-layout))
  "Change the inner pane of the layout description."
  (let ((xs (layout-description layout)))
    (setf (fifth xs) pane)

    ;; change the inner pane
    (apply-in-pane-process layout #'(setf layout-description) xs layout)))

(defmethod (setf padded-layout-top-margin) :after (margin (layout padded-layout))
  "Change the height of the top margin pane."
  (let ((xs (layout-description layout)))
    (setf (second xs) (make-instance 'y-margin-pane :visible-min-height margin))
    
    ;; change the layout
    (apply-in-pane-process layout #'(setf layout-description) xs layout)))

(defmethod (setf padded-layout-right-margin) :after (margin (layout padded-layout))
  "Change the width of the right margin pane."
  (let ((xs (layout-description layout)))
    (setf (sixth xs) (make-instance 'x-margin-pane :visible-min-width margin))
    
    ;; change the layout
    (apply-in-pane-process layout #'(setf layout-description) xs layout)))

(defmethod (setf padded-layout-bottom-margin) :after (margin (layout padded-layout))
  "Change the height of the bottom margin pane."
  (let ((xs (layout-description layout)))
    (setf (eighth xs) (make-instance 'y-margin-pane :visible-min-height margin))
    
    ;; change the layout
    (apply-in-pane-process layout #'(setf layout-description) xs layout)))

(defmethod (setf padded-layout-left-margin) :after (margin (layout padded-layout))
  "Change the height of the left margin pane."
  (let ((xs (layout-description layout)))
    (setf (fourth xs) (make-instance 'x-margin-pane :visible-min-width margin))
    
    ;; change the layout
    (apply-in-pane-process layout #'(setf layout-description) xs layout)))

