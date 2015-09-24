;;;; ***********************************************************************
;;;;
;;;; Name:          state-pick-character.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the pick-character AppState
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 PickCharacterState
 )

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require util-error)
(require data-assets)
(require state)
(require client)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class com.jme3.app.state AbstractAppState AppStateManager))

;;; ---------------------------------------------------------------------
;;; PickCharacterState
;;; ---------------------------------------------------------------------

(define (%pick-character-state-cleanup state::FabricClientState)
  (format #t "~%PickCharacterState cleaned up"))

(define (%pick-character-state-initialize state::FabricClientState)
  (format #t "~%PickCharacterState initialized")
  (set! state:state-initialized? #t))

(define (%pick-character-state-enabled? state::FabricClientState) #t)
(define (%pick-character-state-initialized? state::FabricClientState) state:state-initialized?)
(define (%pick-character-state-attached state::FabricClientState manager::AppStateManager) #!void)
(define (%pick-character-state-detached state::FabricClientState manager::AppStateManager) #!void)

(define (%pick-character-state-handle-analog-event state name value tpf)
  (warn "%pick-character-state-handle-analog-event is not yet implemented"))
(define (%pick-character-state-handle-action-event state name key-pressed? tpf)
  (warn "%pick-character-state-handle-action-event is not yet implemented"))

(define-simple-class PickCharacterState (FabricClientState)
  ;; slots
  ;; methods
  ((cleanup) (%pick-character-state-cleanup (this)))
  ((initialize state-manager::AppStateManager app::FabricClient)
   (begin (invoke-special FabricClientState (this) 'initialize state-manager app)
          (set! app:state (this))
          (%pick-character-state-initialize (this))))
  ((isEnabled) (%pick-character-state-enabled? (this)))
  ((isInitialized) (%pick-character-state-initialized? (this)))
  ((stateAttached state-manager::AppStateManager)
   (%pick-character-state-attached (this) state-manager))
  ((stateDetached state-manager::AppStateManager)
   (%pick-character-state-detached (this) state-manager))
  ((handleAnalogEvent name value tpf)
   (%pick-character-state-handle-analog-event (this) name value tpf))
  ((handleActionEvent name key-pressed? tpf)
   (%pick-character-state-handle-action-event (this) name key-pressed? tpf)))


