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
(require view-faction-picker)
(require view-armor-picker)
(require view-augment-picker)
(require view-weapon-picker)
(require view-name-generator)
(require view-character-acceptor)
(require state)
(require client)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class com.jme3.app.state AbstractAppState AppStateManager))
(import (class com.jme3.scene Node))
(import (class tonegod.gui.controls.text Label))
(import (class tonegod.gui.controls.windows Window))
(import (class tonegod.gui.core Screen))

;;; ---------------------------------------------------------------------
;;; CreateCharacterState
;;; ---------------------------------------------------------------------

(define (%create-character-state-cleanup state::CreateCharacterState)
  (let* ((client::FabricClient state:client)
         (screen::Screen client:screen)
         (gui-node::Node (*:getGuiNode client))
         (nameplate::Label state:faction-nameplate)
         (faction-picker::Window state:faction-picker)
         (armor-picker::Window state:armor-picker)
         (weapon-picker::Window state:weapon-picker)
         (augment-picker::Window state:augment-picker)
         (name-generator::Window state:name-generator)
         (character-acceptor::Window state:character-acceptor))
    (*:removeElement screen nameplate)
    (*:removeElement screen faction-picker)
    (*:removeElement screen armor-picker)
    (*:removeElement screen weapon-picker)
    (*:removeElement screen augment-picker)
    (*:removeElement screen name-generator)
    (*:removeElement screen character-acceptor)
    (*:removeControl gui-node screen))
  #!void)

(define (%create-character-state-initialize state::CreateCharacterState)
  (let* ((client::FabricClient state:client)
         (gui-node::Node (*:getGuiNode client))
         (screen::Screen client:screen)
         (nameplate::Label (make-faction-nameplate screen))
         (faction-picker (make-faction-picker state screen))
         (armor-picker (make-armor-picker state screen))
         (weapon-picker (make-weapon-picker state screen))
         (augment-picker (make-augment-picker state screen))
         (name-generator (make-name-generator state screen))
         (character-acceptor (make-character-acceptor state screen)))
    (set! state:faction-nameplate nameplate)
    (set! state:faction-picker faction-picker)
    (set! state:armor-picker armor-picker)
    (set! state:weapon-picker weapon-picker)
    (set! state:augment-picker augment-picker)
    (set! state:name-generator name-generator)
    (set! state:character-acceptor character-acceptor)
    (*:setText nameplate "Faction: None Chosen")
    (*:addElement screen nameplate)
    (*:addElement screen faction-picker)
    (*:addElement screen armor-picker)
    (*:addElement screen weapon-picker)
    (*:addElement screen augment-picker)
    (*:addElement screen name-generator)
    (*:addElement screen character-acceptor)
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
  (faction-picker init: #!null)
  (armor-picker init: #!null)
  (augment-picker init: #!null)
  (weapon-picker init: #!null)
  (name-generator init: #!null)
  (character-acceptor init: #!null)
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


