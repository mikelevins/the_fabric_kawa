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
(require view-faction-nameplate)
(require state)
(require client)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class com.jme3.app.state AbstractAppState AppStateManager))
(import (class com.jme3.scene Node))
(import (class tonegod.gui.controls.text Label))
(import (class tonegod.gui.core Screen))

;;; ---------------------------------------------------------------------
;;; CreateCharacterState
;;; ---------------------------------------------------------------------

(define (%create-character-state-cleanup state::CreateCharacterState)
  (let* ((client::FabricClient state:client)
         (screen::Screen client:screen)
         (gui-node::Node (*:getGuiNode client))
         (nameplate::Label state:faction-nameplate))
    (*:removeElement screen nameplate)
    (*:removeControl gui-node screen))
  #!void)

(define (%create-character-state-initialize state::CreateCharacterState)
  (let* ((client::FabricClient state:client)
         (gui-node::Node (*:getGuiNode client))
         (screen::Screen client:screen)
         (nameplate::Label (make-faction-nameplate screen)))
    (set! state:faction-nameplate nameplate)
    (*:setText nameplate "Faction: None Chosen")
    (*:addElement screen nameplate)
    (*:addControl gui-node screen)
    (set! state:state-initialized? #t))
  #!void)

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
  (faction-nameplate init: #!null)
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


