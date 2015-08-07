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
 make-create-character-state)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-error.scm")
(require "util-java.scm")
(require "util-lists.scm")
(require "data-nodes.scm")
(require "client-state.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as Application com.jme3.app.Application)
(import-as AppStateManager com.jme3.app.state.AppStateManager)
(import-as BitmapFont com.jme3.font.BitmapFont)
(import-as Label tonegod.gui.controls.text.Label)
(import-as Node com.jme3.scene.Node)
(import-as Screen tonegod.gui.core.Screen)
(import-as Spatial com.jme3.scene.Spatial)
(import-as Vector2f com.jme3.math.Vector2f)
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
  (format #t "~%%create-character-state-detached called"))

(define (make-create-character-state client::Application)
  (let ((state (CreateCharacterState)))
    (*:setClient state client)
    state))

;;; ---------------------------------------------------------------------
;;; CreateCharacterState functions
;;; ---------------------------------------------------------------------

(define (prepare-to-attach-create-character-state state::CreateCharacterState client)
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
        (let* ((client (*:getClient state))
               (screen::Screen (*:getScreen client))
               (gui-node::Node (*:getGuiNode client)))
          (*:enqueue client
                     (runnable (lambda ()
                                 (let ((client (*:getClient state))
                                       (root::Node (*:getRootNode client)))
                                   ;;(*:addElement screen (*:getFactionNameplate state))
                                   ;;(*:addElement screen (*:getCharacterNameplate state))
                                   ;;(*:addElement screen (*:getFactionPicker state))
                                   ;;(*:addElement screen (*:getWeaponPicker state))
                                   ;;(*:addElement screen (*:getArmorPicker state))
                                   ;;(*:addElement screen (*:getAugmentPicker state))
                                   ;;(*:addElement screen (*:getNamePicker state))
                                   ;;(*:addElement screen (*:getCharacterAcceptor state))
                                   (*:addControl gui-node screen))))))))
