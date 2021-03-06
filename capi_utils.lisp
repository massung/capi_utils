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

   ;; platform-independent functions
   #:get-user-name

   ;; os x functions
   #+cocoa #:hide-application
   #+cocoa #:hide-other-applications
   #+cocoa #:unhide-application
   #+cocoa #:unhide-all-applications
   #+cocoa #:open-file-with-application

   ;; search-text-pane
   #:search-text-pane
   #:search-text-pane-last-search
   #:search-text-pane-soft-search-callback
   #:search-text-pane-hard-search-callback
   #:search-text-pane-perform-soft-search
   #:search-text-pane-perform-hard-search
   #:search-text-pane-placeholder-text

   ;; padded-layout
   #:padded-layout
   #:padded-layout-inner-pane
   #:padded-layout-top-margin
   #:padded-layout-right-margin
   #:padded-layout-bottom-margin
   #:padded-layout-left-margin
   ))

(in-package :capi-utils)

