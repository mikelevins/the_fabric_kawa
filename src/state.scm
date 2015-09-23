;;;; ***********************************************************************
;;;;
;;;; Name:          state.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       FabricClientState
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 FabricClientState
 )

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require util-error)
(require data-assets)
(require client)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class com.jme3.app.state AbstractAppState AppStateManager))

;;; ---------------------------------------------------------------------
;;; FabricClient
;;; ---------------------------------------------------------------------


(define (%state-cleanup state::FabricClientState) #!void)
(define (%state-initialize state::FabricClientState) #!void)
(define (%state-enabled? state::FabricClientState) #t)
(define (%state-initialized? state::FabricClientState) state:state-initialized?)
(define (%state-attached state::FabricClientState manager::AppStateManager) #!void)
(define (%state-detached state::FabricClientState manager::AppStateManager) #!void)

;;; CLASS FabricClientState
;;; ---------------------------------------------------------------------

(define-simple-class FabricClientState (AbstractAppState)
  ;; slots
  (client init: #!null)
  (state-initialized? init: #f)
  ;; methods
  ((cleanup) (%state-cleanup (this)))
  ((initialize state-manager::AppStateManager app::FabricClient)
   (begin (invoke-special AbstractAppState (this) 'initialize state-manager app)
          (set! client app)
          (%state-initialize (this))))
  ((isEnabled) (%state-enabled? (this)))
  ((isInitialized) (%state-initialized? (this)))
  ((stateAttached state-manager::AppStateManager)
   (%state-attached (this) state-manager))
  ((stateDetached state-manager::AppStateManager)
   (%state-detached (this) state-manager))
  ((handleAnalogEvent name value tpf)
   (format #t "~%handleAnalogEvent must be implemented in a subclass of FabricClientState"))
  ((handleActionEvent name key-pressed? tpf)
   (format #t "~%handleActionEvent must be implemented in a subclass of FabricClientState")))
