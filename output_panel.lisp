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

(defclass output-panel (output-pane choice)
  ((item-display-callback :initform nil :initarg :item-display-callback :accessor output-panel-item-display-callback)
   (item-action-callback  :initform nil :initarg :item-action-callback  :accessor output-panel-item-action-callback)
   (item-height           :initform 20  :initarg :item-height           :accessor output-panel-item-height)
   (item-menu             :initform nil :initarg :item-menu             :accessor output-panel-item-menu)

   ;; selected item settings
   (selected-bg           :initform nil :initarg :selected-background   :accessor output-panel-selected-background)
   (selected-fg           :initform nil :initarg :selected-foreground   :accessor output-panel-selected-foreground)

   ;; the last-clicked item
   (last-selected-index   :initform nil :initarg :last-selected-index   :accessor output-panel-last-selected-index))
  (:default-initargs
   :draw-with-buffer t
   :vertical-scroll t
   :pane-can-scroll t
   :scroll-start-y 0
   :scroll-height 0
   :resize-callback 'resize-output-panel
   :display-callback 'display-output-panel
   :scroll-callback 'scroll-output-panel
   :input-model '(((:button-1 :press) click-item)
                  ((:button-1 :second-press) double-click-item)
                  ((:button-1 :press :shift) shift-click-item)
                  ((:button-1 :press #+cocoa :hyper #+mswindows :control) hyper-click-item)
                  ;((:motion :button-1 :press) drag-item)
                  (:post-menu post-menu-item))))

(defmethod resize-scroll ((panel output-panel))
  "Calculate the new scroll height from the collection size."
  (with-geometry panel
    (setf %scroll-height% (* (output-panel-item-height panel) (count-collection-items panel)))))

(defmethod resize-output-panel ((panel output-panel) x y w h)
  "Recalculate the scroll size and redraw."
  (resize-scroll panel)
  (gp:invalidate-rectangle panel))

(defmethod display-output-panel ((panel output-panel) bx by bw bh)
  "Render all visible items in the panel."
  (let* ((pos (get-vertical-scroll-parameters panel :slug-position))

         ;; panel visible width and height
         (w (simple-pane-visible-width panel))
         (h (simple-pane-visible-height panel))

         ;; the render mask so items don't render outside their area
         (mask (list 0 0 w h))

         ;; height per item
         (ih (output-panel-item-height panel)))

    ;; clear the panel
    (gp:clear-graphics-port panel)

    ;; determine the start index and the negative y offset
    (loop with (start offset) = (multiple-value-list (truncate pos ih))
          with n = (count-collection-items panel)
          
          ;; background and foreground
          with bg = (simple-pane-background panel)
          with fg = (simple-pane-foreground panel)
          
          ;; get the default background and foreground colors for selected items
          with sel-bg = (or (output-panel-selected-background panel) :color_highlight)
          with sel-fg = (or (output-panel-selected-foreground panel) :color_highlighttext)
          
          ;; loop over each item
          for i from start
          for y from (- offset) by ih
          
          ;; stop when offscreen or at the end of the collection
          until (or (> y h) (>= i n))
          
          ;; translate, mask, set colors, and draw
          do (let* ((item (get-collection-item panel i))
                    (selected (choice-selected-item-p panel item))
                    
                    ;; set the translation based on the scroll position
                    (tform (gp:make-transform 1 0 0 1 0 y))
                    
                    ;; pick the background and foreground to use
                    (bg (if selected sel-bg bg))
                    (fg (if selected sel-fg fg)))
               (gp:with-graphics-state (panel :mask mask :transform tform :background bg :foreground fg)
                 (gp:draw-rectangle panel 0 0 w ih :filled t :foreground bg)
                 (when-let (callback (output-panel-item-display-callback panel))
                   (funcall callback panel item w ih selected)))))))

(defmethod scroll-output-panel ((panel output-panel) direction op value &key interactive)
  "The user is scrolling, so update the scroll position and redraw."
  (declare (ignore interactive))
  (let ((y (get-vertical-scroll-parameters panel :slug-position)))
    (case op
      (:move (set-vertical-scroll-parameters panel :slug-position value))
      (:drag (set-vertical-scroll-parameters panel :slug-position value))
      
      ;; relative by a single item
      (:step (let ((step (* value (output-panel-item-height panel))))
               (set-vertical-scroll-parameters panel :slug-position (+ y step))))

      ;; relateive by a single page
      (:page (let ((page (* value (simple-pane-visible-height panel))))
               (set-vertical-scroll-parameters panel :slug-position (+ y page))))))
  (gp:invalidate-rectangle panel))

(defmethod select-item ((panel output-panel) item &key single-selection-p)
  "Add or remove an item from the current selection set."
  (when-let (i (search-for-item panel item))
    (when (setf (choice-selected-items panel)
                (cond ((member (choice-interaction panel) '(:no-selection nil)))
                      
                      ;; single selection or forced single selection
                      ((or single-selection-p (eq (choice-interaction panel) :single-selection))
                       (list item))
                      
                      ;; item already selected?
                      ((choice-selected-item-p panel item)
                       (remove item (choice-selected-items panel)))
                      
                      ;; multiple or extended selection
                      (t (cons item (choice-selected-items panel)))))
      (setf (output-panel-last-selected-index panel) i))))

(defmethod extend-item-selection ((panel output-panel) item)
  "Select a range of items from the last selected item to this one."
  (case (choice-interaction panel)
    ((:no-selection nil))

    ;; don't extend, just select this item
    (:single-selection (select-item panel item))

    ;; multiple or extended selection
    (otherwise (if-let (j (output-panel-last-selected-index panel))
                   (setf (choice-selection panel)
                         (loop with i = (search-for-item panel item)
                               for n from (min i j) to (max i j)
                               collect n))
                 (select-item panel item)))))

(defmethod item-at-position ((panel output-panel) x y)
  "Return the item clicked at a given position."
  (with-geometry panel
    (when (and (< 0 x %width%)
               (< 0 y %scroll-height%))
      (get-collection-item panel (truncate y (output-panel-item-height panel))))))

(defmethod click-item ((panel output-panel) x y)
  "Select an item."
  (when-let (item (item-at-position panel x y))
    (select-item panel item :single-selection-p t)))

(defmethod double-click-item ((panel output-panel) x y)
  "Select and perform an action on a given item."
  (when-let (item (item-at-position panel x y))
    (select-item panel item :single-selection-p t)
    (when-let (callback (output-panel-item-action-callback panel))
      (funcall callback panel item))))

(defmethod shift-click-item ((panel output-panel) x y)
  "Select a range of items."
  (when-let (item (item-at-position panel x y))
    (extend-item-selection panel item)))

(defmethod hyper-click-item ((panel output-panel) x y)
  "Toggle the selection status of an item."
  (when-let (item (item-at-position panel x y))
    (select-item panel item)))

(defmethod post-menu-item ((panel output-panel) x y)
  "Display an alternative action menu for the current selection."
  (when-let (item (item-at-position panel x y))
    (unless (choice-selected-item-p panel item)
      (select-item panel item :single-selection-p t))

    ;; show the menu
    (when-let (menu (output-panel-item-menu panel))
      (display-popup-menu (funcall menu (top-level-interface panel)) :output panel :x x :y y))))

(defmethod (setf collection-items) (items (panel output-panel))
  "Update the scroll height and maintain the current selection across item sets."
  (let ((selection (choice-selected-items panel)))

    ;; allow the items to change now...
    (call-next-method items panel)
    
    ;; resize the scrollbar
    (resize-scroll panel)
  
    ;; if the slug position is past the end, truncate it
    (with-geometry panel
      (unless (<= (get-vertical-scroll-parameters panel :slug-position) %scroll-height%)
        (set-vertical-scroll-parameters panel :slug-position %scroll-height%)))

    ;; assign the selection back
    (if (sequencep selection)
        (setf (choice-selected-items panel) selection)
      (setf (choice-selected-item panel) selection))))

(defmethod (setf choice-selection) :after (indices (panel output-panel))
  "The selection changed, redraw the panel."
  (gp:invalidate-rectangle panel))
