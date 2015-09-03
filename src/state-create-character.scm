;;;; ***********************************************************************
;;;;
;;;; Name:          state-create-character.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       fabric AppStates
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************
(format #t "~%loading state-create-character.scm")

(module-export
 CreateCharacterState
 compute-armor-picker-rect
 compute-augment-picker-rect
 compute-faction-picker-rect
 compute-name-picker-rect
 compute-weapon-picker-rect
 make-create-character-state)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-error.scm")
(require "util-java.scm")
(require "util-lists.scm")
(require "data-nodes.scm")
(require "model-rect.scm")
(require "client-state.scm")
(require "client-class.scm")
(require "view-skybox.scm")
(require "view-character-nameplate.scm")
(require "view-faction-nameplate.scm")
(require "view-character-acceptor.scm")
(require "view-armor-picker.scm")
(require "view-augment-picker.scm")
(require "view-faction-picker.scm")
(require "view-name-picker.scm")
(require "view-weapon-picker.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------


(import-as AbstractAppState com.jme3.app.state.AbstractAppState)
(import-as AppStateManager com.jme3.app.state.AppStateManager)
(import-as Application com.jme3.app.Application)
(import-as BitmapFont com.jme3.font.BitmapFont)
(import-as ButtonAdapter tonegod.gui.controls.buttons.ButtonAdapter)
(import-as Client com.jme3.network.Client)
(import-as ConnectException java.net.ConnectException)
(import-as DefaultClient com.jme3.network.base.DefaultClient)
(import-as Label tonegod.gui.controls.text.Label)
(import-as Network com.jme3.network.Network)
(import-as Node com.jme3.scene.Node)
(import-as Screen tonegod.gui.core.Screen)
(import-as Serializer com.jme3.network.serializing.Serializer)
(import-as SimpleApplication com.jme3.app.SimpleApplication)
(import-as Spatial com.jme3.scene.Spatial)
(import-as Vector2f com.jme3.math.Vector2f)
(import-as Vector4f com.jme3.math.Vector4f)
(import-as Window tonegod.gui.controls.windows.Window)

;;; ---------------------------------------------------------------------
;;; the client character-creator AppState class
;;; ---------------------------------------------------------------------

;;; CLASS CreateCharacterState
;;; ---------------------------------------------------------------------

(define-simple-class CreateCharacterState (FabricClientState)
  ;; slots
  (initialized? init: #f)
  ((getInitialized) initialized?)
  ((setInitialized newstate) (set! initialized? newstate))
  ;; methods
  ((cleanup) (%create-character-state-cleanup (this)))
  ((isEnabled) (%create-character-state-enabled? (this)))
  ((stateAttached state-manager::AppStateManager)
   (%create-character-state-attached (this) state-manager))
  ((stateDetached state-manager::AppStateManager)
   (%create-character-state-detached (this) state-manager))
  ;; init
  ((initialize) (%create-character-state-initialize (this)))
  ((isInitialized) (%create-character-state-initialized? (this))))

(define (%create-character-state-cleanup state::CreateCharacterState)
  (format #t "~%%create-character-state-cleanup called"))

(define (%create-character-state-initialize state::CreateCharacterState)
  (format #t "~%%create-character-state-initialize called"))

(define (%create-character-state-enabled? state::CreateCharacterState) #t)

(define (%create-character-state-initialized? state::CreateCharacterState) #t)

(define (%create-character-state-attached state::CreateCharacterState manager::AppStateManager)
  (let ((client::Application (*:getClient state)))
    (prepare-to-attach-create-character-state state client)
    (did-attach-create-character-state state manager)))

(define (%create-character-state-detached state::CreateCharacterState manager::AppStateManager)
  (did-detach-create-character-state state manager))

(define (make-create-character-state client::Application)
  (let ((state (CreateCharacterState)))
    (*:setClient state client)
    state))

;;; ---------------------------------------------------------------------
;;; CreateCharacterState functions
;;; ---------------------------------------------------------------------

;;; layout computations

(define (compute-faction-picker-rect screen::Screen)
  (make-rectangle 8 8 640 200))

(define (compute-weapon-picker-rect screen::Screen)
  (let* ((faction-picker-rect (compute-faction-picker-rect screen))
         (weapon-picker-left (get-left faction-picker-rect))
         (weapon-picker-top (+ (get-top faction-picker-rect)
                               (get-height faction-picker-rect)
                               8))
         (weapon-picker-width 256)
         (weapon-picker-height 720))
    (make-rectangle weapon-picker-left
                    weapon-picker-top
                    weapon-picker-width
                    weapon-picker-height)))

(define (compute-armor-picker-rect screen::Screen)
  (let* ((weapon-picker-rect (compute-weapon-picker-rect screen))
         (screen-width (*:getWidth screen))
         (armor-picker-width (get-width weapon-picker-rect))
         (armor-picker-height (get-height weapon-picker-rect))
         (armor-picker-left (- screen-width (+ armor-picker-width 8)))
         (armor-picker-top (get-top weapon-picker-rect)))
    (make-rectangle armor-picker-left
                    armor-picker-top
                    armor-picker-width
                    armor-picker-height)))

(define (compute-augment-picker-rect screen::Screen)
  (let* ((faction-picker-rect (compute-faction-picker-rect screen))
         (screen-width (*:getWidth screen))
         (augment-picker-width (get-width faction-picker-rect))
         (augment-picker-height (get-height faction-picker-rect))
         (augment-picker-left (- screen-width augment-picker-width 8))
         (augment-picker-top (get-top faction-picker-rect)))
    (make-rectangle augment-picker-left
                    augment-picker-top
                    augment-picker-width
                    augment-picker-height)))


(define (compute-name-picker-rect screen::Screen)
  (let* ((weapon-picker-rect (compute-weapon-picker-rect screen))
         (screen-width (*:getWidth screen))
         (screen-height (*:getHeight screen))
         (picker-width (- screen-width (+ 24 (get-width weapon-picker-rect))))
         (picker-height 220)
         (picker-top (- screen-height
                        (+ picker-height 8)))
         (picker-left 8))
    (make-rectangle picker-left
                    picker-top
                    picker-width
                    picker-height)))

;;; attach and detach

(define (prepare-to-attach-create-character-state state::CreateCharacterState client::FabricClient)
  (unless (*:getInitialized state)
    (let* ((screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client))
           (Align BitmapFont:Align)
           ;;(faction-nameplate::Label (make-faction-nameplate screen))
           ;;(character-nameplate::Label (make-character-nameplate screen))
           ;;(faction-picker::Window (make-faction-picker state screen))
           ;;(weapon-picker::Window (make-weapon-picker state screen))
           ;;(armor-picker::Window (make-armor-picker state screen))
           ;;(augment-picker::Window (make-augment-picker state screen))
           ;;(name-picker::Window (make-name-picker screen))
           ;;(character-acceptor::Window (make-character-acceptor screen))
           )
      ;;(*:setFactionPicker state faction-picker)
      ;;(*:setWeaponPicker state weapon-picker)
      ;;(*:setArmorPicker state armor-picker)
      ;;(*:setAugmentPicker state augment-picker)
      ;;(*:setNamePicker state name-picker)
      ;;(*:setCharacterAcceptor state character-acceptor)
      ;;(*:setFactionNameplate state faction-nameplate)
      ;;(*:setCharacterNameplate state character-nameplate)
      (*:setInitialized state #t))))

(define (did-attach-create-character-state state::CreateCharacterState mgr::AppStateManager)
  (when (*:getInitialized state)
    (let* ((client::FabricClient (*:getClient state))
           (screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client)))
      (*:enqueue client
                 (runnable (lambda ()
                             (let ((root::Node (*:getRootNode client)))
                               ;;(*:addElement screen (*:getFactionNameplate state))
                               ;;(*:addElement screen (*:getCharacterNameplate state))
                               ;;(*:addElement screen (*:getFactionPicker state))
                               ;;(*:addElement screen (*:getWeaponPicker state))
                               ;;(*:addElement screen (*:getArmorPicker state))
                               ;;(*:addElement screen (*:getAugmentPicker state))
                               ;;(*:addElement screen (*:getNamePicker state))
                               ;;(*:addElement screen (*:getCharacterAcceptor state))
                               (*:addControl gui-node screen))))))))

(define (did-detach-create-character-state state::CreateCharacterState mgr::AppStateManager)
  (when (*:getInitialized state)
    (let* ((client::FabricClient (*:getClient state))
           (screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client)))
      (*:enqueue client
                 (runnable (lambda ()
                             (let ((root::Node (*:getRootNode client)))
                               ;;(*:addElement screen (*:getFactionNameplate state))
                               ;;(*:addElement screen (*:getCharacterNameplate state))
                               ;;(*:addElement screen (*:getFactionPicker state))
                               ;;(*:addElement screen (*:getWeaponPicker state))
                               ;;(*:addElement screen (*:getArmorPicker state))
                               ;;(*:addElement screen (*:getAugmentPicker state))
                               ;;(*:addElement screen (*:getNamePicker state))
                               ;;(*:addElement screen (*:getCharacterAcceptor state))
                               (*:removeControl gui-node screen))))))))
