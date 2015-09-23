;;;; ***********************************************************************
;;;;
;;;; Name:          state-create-character.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the create-character AppState
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 CreateCharacterState
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
;;; CreateCharacterState
;;; ---------------------------------------------------------------------

(define (%create-character-state-cleanup state::FabricClientState)
  (format #t "~%CreateCharacterState cleaned up"))

(define (%create-character-state-initialize state::FabricClientState)
  (format #t "~%CreateCharacterState initialized")
  (set! state:state-initialized? #t))

(define (%create-character-state-enabled? state::FabricClientState) #t)
(define (%create-character-state-initialized? state::FabricClientState) state:state-initialized?)
(define (%create-character-state-attached state::FabricClientState manager::AppStateManager) #!void)
(define (%create-character-state-detached state::FabricClientState manager::AppStateManager) #!void)
(define (%create-character-state-handle-analog-event state name value tpf)
  (warn "%create-character-state-handle-analog-event is not yet implemented"))
(define (%create-character-state-handle-action-event state name key-pressed? tpf)
  (warn "%create-character-state-handle-action-event is not yet implemented"))

(define-simple-class CreateCharacterState (FabricClientState)
  ;; slots
  ;; methods
  ((cleanup) (%create-character-state-cleanup (this)))
  ((initialize state-manager::AppStateManager app::FabricClient)
   (begin (invoke-special FabricClientState (this) 'initialize state-manager app)
          (set! app:state (this))
          (%create-character-state-initialize (this))))
  ((isEnabled) (%create-character-state-enabled? (this)))
  ((isInitialized) (%create-character-state-initialized? (this)))
  ((stateAttached state-manager::AppStateManager)
   (%create-character-state-attached (this) state-manager))
  ((stateDetached state-manager::AppStateManager)
   (%create-character-state-detached (this) state-manager))
  ((handleAnalogEvent name value tpf)
   (%create-character-state-handle-analog-event (this) name value tpf))
  ((handleActionEvent name key-pressed? tpf)
   (%create-character-state-handle-action-event (this) name key-pressed? tpf)))


