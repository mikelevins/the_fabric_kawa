;;;; ***********************************************************************
;;;;
;;;; Name:          state-transit.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       fabric AppStates
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************
(format #t "~%loading state-transit.scm")

(module-export
 TransitState
 make-transit-state)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-error.scm")
(require "util-java.scm")
(require "util-lists.scm")
(require "data-nodes.scm")
(require "syntax-classes.scm")
(require "client-state.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as Application com.jme3.app.Application)
(import-as AppStateManager com.jme3.app.state.AppStateManager)

;;; ---------------------------------------------------------------------
;;; the client transit AppState class
;;; ---------------------------------------------------------------------

;;; CLASS TransitState
;;; ---------------------------------------------------------------------

(defclass TransitState (FabricClientState)
  (slots:
   (initialized? init-form: #f getter: getInitialized setter: setInitialized)
   (from-name init-form: #f getter: getFromName setter: setFromName)
   (to-name init-form: #f getter: getToName setter: setToName)) 
  (methods:
   ((cleanup) (%transit-client-state-cleanup (this)))
   ((initialize) (%transit-client-state-initialize (this)))
   ((isEnabled) #t)
   ((isInitialized) (*:getInitialized (this)))
   ((stateAttached state-manager::AppStateManager) (%transit-client-state-attached (this) state-manager))
   ((stateDetached state-manager::AppStateManager) (%transit-client-state-detached (this) state-manager))))

(define (%transit-client-state-cleanup state::TransitState)
  #!void)

(define (%transit-client-state-initialize state::TransitState)
  #!void)

(define (%transit-client-state-attached state::TransitState manager::AppStateManager)
  #!void)

(define (%transit-client-state-detached state::TransitState manager::AppStateManager)
  #!void)

(define (make-transit-state client::Application from-name to-name)
  (let ((state (TransitState)))
    (*:setClient state client)
    (*:setFromName state from-name)
    (*:setToName state to-name)
    state))
