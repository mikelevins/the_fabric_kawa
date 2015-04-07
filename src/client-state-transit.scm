;;;; ***********************************************************************
;;;;
;;;; Name:          client-state-transit.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       fabric AppStates
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************
(format #t "~%loading client-state-transit.scm")

(module-export
 TransitClientState
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

;;; CLASS TransitClientState
;;; ---------------------------------------------------------------------

(defclass TransitClientState (FabricClientState)
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

(define (%transit-client-state-cleanup state::TransitClientState)
  #!void)

(define (%transit-client-state-initialize state::TransitClientState)
  #!void)

(define (%transit-client-state-attached state::TransitClientState manager::AppStateManager)
  #!void)

(define (%transit-client-state-detached state::TransitClientState manager::AppStateManager)
  #!void)

(define (make-transit-state client::Application from-name to-name)
  (let ((state (TransitClientState)))
    (*:setClient state client)
    (*:setFromName state from-name)
    (*:setToName state to-name)
    state))
