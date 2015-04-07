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
   ((cleanup) (%transit-state-cleanup (this)))
   ((initialize) (%transit-state-initialize (this)))
   ((isEnabled) (%transit-state-enabled? (this)))
   ((isInitialized) (%transit-state-initialized? (this)))
   ((stateAttached state-manager::AppStateManager)
    (%transit-state-attached (this) state-manager))
   ((stateDetached state-manager::AppStateManager)
    (%transit-state-detached (this) state-manager))))

(define (%transit-state-cleanup state::TransitState) #!void)
(define (%transit-state-initialize state::TransitState) #!void)
(define (%transit-state-enabled? state::TransitState) #t)
(define (%transit-state-initialized? state::TransitState) #t)
(define (%transit-state-attached state::TransitState manager::AppStateManager) #!void)
(define (%transit-state-detached state::TransitState manager::AppStateManager) #!void)

(define (make-transit-state client::Application #!key (from "The Sun")(to "Earth"))
  (let ((state (TransitState)))
    (*:setClient state client)
    (*:setFromName state from)
    (*:setToName state to)
    state))
