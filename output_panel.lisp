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

(defclass output-panel (output-pane collection)
  ((item-height   :initform 20  :initarg :item-height            :accessor output-panel-item-height)
   (item-menu     :initform nil :initarg :item-menu              :accessor output-panel-item-menu)
   
   ;; item interaction callbacks
   (item-display  :initform nil :initarg :item-display-callback  :accessor output-panel-item-display-callback)
   (item-action   :initform nil :initarg :item-action-callback   :accessor output-panel-item-action-callback)
   (item-selected :initform nil :initarg :item-selected-callback :accessor output-panel-item-select-callback)
   (item-retract  :initform nil :initarg :item-retract-callback  :accessor output-panel-item-retract-callback)

   ;; selected item settings
   (selected-bg   :initform nil :initarg :selected-background    :accessor output-panel-selected-background)
   (selected-fg   :initform nil :initarg :selected-foreground    :accessor output-panel-selected-foreground)

   ;; interaction mode (:no-selection, :single-selection, :multiple-selection)
   (interaction   :initform nil :initarg :interaction            :accessor output-panel-interaction)

   ;; currently selected indices and callback
   (selection     :initform nil :initarg :selected-items         :reader   output-panel-selection)
   (selection-cb  :initform nil :initarg :selection-callback     :accessor output-panel-selection-callback))
  (:default-initargs
   :draw-with-buffer t
   :vertical-scroll t
   :pane-can-scroll t
   :scroll-start-y 0
   :scroll-height 0
   :test-function 'equal
   :resize-callback 'resize-output-panel
   :display-callback 'display-output-panel
   :scroll-callback 'scroll-output-panel
   :input-model '(((:button-1 :press) click-item)
                  ((:button-1 :second-press) double-click-item)
                  ((:button-1 :press :shift) shift-click-item)
                  ((:button-1 :press #+cocoa :hyper #+mswindows :control) hyper-click-item)
                  ;((:motion :button-1 :press) drag-item)
                  (:post-menu post-menu-item))))

(defmethod apply-callback ((panel output-panel) callback-slot item &rest args)
  "Send the callback the arguments in callback-type."
  (when-let (callback (slot-value panel callback-slot))
    (apply callback panel item args)))

(defmethod resize-scroll ((panel output-panel))
  "Calculate the new scroll height from the collection size."
  (let ((h (* (output-panel-item-height panel) (count-collection-items panel))))

    ;; change the maximum range of the panel
    (set-vertical-scroll-parameters panel :max-range h)

    ;; if everything fits just fine, set the slug back to 0
    (if (<= h (simple-pane-visible-height panel))
        (set-vertical-scroll-parameters panel :slug-position 0)
      (unless (<= (or (get-vertical-scroll-parameters panel :slug-position) 0) h)
        (set-vertical-scroll-parameters panel :slug-position h)))))

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

                    ;; the render mask so items don't render outside their area
                    (mask (list 0 y w ih))
                    
                    ;; pick the background and foreground to use
                    (bg (if selected sel-bg bg))
                    (fg (if selected sel-fg fg)))
               (gp:with-graphics-state (panel :mask mask :transform tform :background bg :foreground fg)
                 (gp:draw-rectangle panel 0 0 w ih :filled t :foreground bg)

                 ;; allow the item to draw itself
                 (apply-callback panel 'item-display item w ih selected))))))

(defmethod scroll-output-panel ((panel output-panel) direction op value &key interactive)
  "The user is scrolling, so update the scroll position and redraw."
  (when interactive
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
    
    ;; redraw since the slug position changed
    (gp:invalidate-rectangle panel)))

(defmethod select-index ((panel output-panel) i &key single-selection-p)
  "Add or remove an item from the current selection set."
  (setf (output-panel-selection panel)
        (cond ((member (output-panel-interaction panel) '(:no-selection nil))
               ())
              
              ;; single selection or forced single selection
              ((or single-selection-p (eq (output-panel-interaction panel) :single-selection))
               (list i))
              
              ;; item already selected?
              ((member i (output-panel-selection panel))
               (remove i (output-panel-selection panel)))
              
              ;; multiple or extended selection
              (t (cons i (output-panel-selection panel))))))

(defmethod extend-index-selection ((panel output-panel) i)
  "Select a range of items from the last selected item to this one."
  (case (output-panel-interaction panel)
    ((:no-selection nil))
    
    ;; don't extend, just select this item
    (:single-selection (setf (output-panel-selection panel) (list i)))
    
    ;; multiple or extended selection (ensure this item is first in the selection list)
    (otherwise (if-let (j (first (output-panel-selection panel)))
                   (setf (output-panel-selection panel)
                         (if (> i j)
                             (loop for n from j to i collect n)
                           (loop for n from j downto i collect n)))
                 (select-index panel i)))))

(defmethod index-at-position ((panel output-panel) x y)
  "Return the item clicked at a given position."
  (with-geometry panel
    (when (and (< 0 x %width%)
               (< 0 y %scroll-height%))
      (truncate y (output-panel-item-height panel)))))

(defmethod click-item ((panel output-panel) x y)
  "Select an item."
  (when-let (i (index-at-position panel x y))
    (select-index panel i :single-selection-p t)))

(defmethod double-click-item ((panel output-panel) x y)
  "Select and perform an action on a given item."
  (when-let (i (index-at-position panel x y))
    (select-index panel i :single-selection-p t)
    
    ;; let the item do something since it was acted on
    (apply-callback panel 'item-action (get-collection-item panel i))))

(defmethod shift-click-item ((panel output-panel) x y)
  "Select a range of items."
  (when-let (i (index-at-position panel x y))
    (extend-index-selection panel i)))

(defmethod hyper-click-item ((panel output-panel) x y)
  "Toggle the selection status of an item."
  (when-let (i (index-at-position panel x y))
    (select-index panel i)))

(defmethod post-menu-item ((panel output-panel) x y)
  "Display an alternative action menu for the current selection."
  (when-let (i (index-at-position panel x y))
    (unless (member i (output-panel-selection panel))
      (select-index panel i :single-selection-p t))
      
    ;; show the menu
    (when-let (menu (output-panel-item-menu panel))
      (display-popup-menu (funcall menu (top-level-interface panel)) :owner panel :x x :y y))))
  
(defmethod output-panel-selected-item-p ((panel output-panel) item)
  "T if the item is currently selected."
  (loop with test = (collection-test-function panel)

        ;; loop over the selected indices
        for i in (output-panel-selection panel)
        for selected-item = (get-collection-item panel i)

        ;; success if the items match
        when (funcall test item selected-item) return t))

(defmethod output-panel-selected-items ((panel output-panel))
  "Return the list of selected items from the selection set."
  (loop for i in (output-panel-selection panel) collect (get-collection-item panel i)))

(defmethod output-panel-sort ((panel output-panel) predicate &key key)
  "Sort the colleciton items."
  (setf (collection-items panel) (stable-sort (collection-items panel) predicate :key key)))

(defmethod output-panel-select-all ((panel output-panel))
  "Select all the items."
  (when (member (output-panel-interaction panel) '(:multiple-selection :extended-selection))
    (setf (output-panel-selection panel)
          (loop for i below (count-collection-items panel) collect i))))

(defmethod (setf output-panel-selected-items) (items (panel output-panel))
  "Set the selection by item instead of index."
  (setf (output-panel-selection panel)

        ;; only keep items that are actually in the collection
        (loop for item in items for i = (search-for-item panel item) when i collect i)))

(defmethod (setf collection-items) (items (panel output-panel))
  "Update the scroll height and maintain the current selection across item sets."
  (let ((selection (output-panel-selected-items panel)))

    ;; allow the items to change now...
    (call-next-method items panel)

    ;; re-select the same objects without issuing callbacks or a selection changed
    (setf (slot-value panel 'selection)
          (loop for item in selection for i = (search-for-item panel item) when i collect i)))
  
  ;; resize the scrollbar
  (resize-scroll panel)

  ;; redraw
  (gp:invalidate-rectangle panel))

(defmethod (setf output-panel-selection) (indices (panel output-panel))
  "The selection is about to change, inform items, inform the panel, and redraw."
  (let* ((selection (output-panel-selection panel))
         
         ;; determine which selected items are going and are new
         (selected (set-difference indices selection))
         (retracted (set-difference selection indices)))

    ;; update the selection - make sure the indices are valid
    (setf (slot-value panel 'selection)
          (loop for i in indices when (< -1 i (count-collection-items panel)) collect i))

    ;; issue selected callbacks for newly selected items
    (loop for i in selected do (apply-callback panel 'item-selected (get-collection-item panel i)))

    ;; issue retracted callbacks for items that used to be selected
    (loop for i in retracted do (apply-callback panel 'item-retract (get-collection-item panel i)))

    ;; notify that the selection changed if it has
    (when (or selected retracted)
      (when-let (callback (output-panel-selection-callback panel))
        (funcall callback panel))))

  ;; redraw
  (gp:invalidate-rectangle panel))

(defmethod (setf output-panel-interaction) :after (mode (panel output-panel))
  "The interaction mode changed. Maybe change the selection."
  (setf (output-panel-selection panel)
        (let ((selection (output-panel-selection panel)))
          (case mode

            ;; single selection, keep the first item
            (:single-selection (and selection (first selection)))

            ;; it's multiple select, so just keep everything
            (:multiple-selection selection)))))
