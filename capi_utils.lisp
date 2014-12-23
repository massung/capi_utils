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

(defpackage :capi-utils
  (:use :cl :lw :capi :fli)
  (:export

   ;; search-text-pane
   #:search-text-pane
   #:search-text-pane-last-search
   #:search-text-pane-live-search-p
   #:search-text-pane-search-callback
   #:search-text-pane-perform-search
   #:search-text-pane-placeholder-text

   ;; output panels
   #:output-panel
   #:output-panel-item-display-callback
   #:output-panel-item-action-callback
   #:output-panel-item-height
   #:output-panel-item-menu
   ))

(in-package :capi-utils)

