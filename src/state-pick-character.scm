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
 add-character-picker-button!)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require util-error)
(require data-assets)
(require state)
(require client)
(require view-character-picker)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class com.jme3.app.state AbstractAppState AppStateManager))
(import (class com.jme3.scene Node))
(import (class tonegod.gui.controls.buttons RadioButton))
(import (class tonegod.gui.controls.windows Window))
(import (class tonegod.gui.core Screen))

;;; ---------------------------------------------------------------------
;;; PickCharacterState
;;; ---------------------------------------------------------------------

(define (%pick-character-state-cleanup state::PickCharacterState)
  (let* ((client::FabricClient state:client)
         (screen::Screen client:screen)
         (picker::Window state:picker)
         (gui-node::Node (*:getGuiNode client)))
    (*:removeElement screen picker)
    (*:removeControl gui-node screen)
    (set! state:picker-buttons '())
    (set! state:picker #!null)))

(define (%pick-character-state-initialize state::PickCharacterState)
  (let* ((client::FabricClient state:client)
         (screen::Screen client:screen)
         (picker::Window (make-character-picker state screen))
         (gui-node::Node (*:getGuiNode client)))
    (set! state:picker picker)
    (*:addElement screen picker)
    (*:addControl gui-node screen)
    (set! state:state-initialized? #t)))

(define (%pick-character-state-enabled? state::FabricClientState) #t)
(define (%pick-character-state-initialized? state::FabricClientState) state:state-initialized?)
(define (%pick-character-state-attached state::FabricClientState manager::AppStateManager) #!void)
(define (%pick-character-state-detached state::FabricClientState manager::AppStateManager) #!void)

(define (%pick-character-state-handle-analog-event state name value tpf)
  ;;; TODO: implement event handling
  #!void)

(define (%pick-character-state-handle-action-event state name key-pressed? tpf)
  ;;; TODO: implement event handling
  #!void)

(define-simple-class PickCharacterState (FabricClientState)
  ;; slots
  (picker init: #!null)
  (picker-buttons init: '())
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

(define (add-character-picker-button! state::PickCharacterState button::RadioButton)
  (set! state:picker-buttons
        (cons button state:picker-buttons)))

