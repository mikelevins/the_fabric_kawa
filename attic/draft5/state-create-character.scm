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
 compute-name-generator-rect
 compute-name-picker-rect
 compute-weapon-picker-rect
 make-create-character-state)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require util-error)
(require util-color)
(require util-lists)
(require data-nodes)
(require model-rect)
(require model-character)
(require syntax-events)
(require client-state)
(require client-class)
(require view-skybox)
(require view-character-nameplate)
(require view-faction-nameplate)
(require view-character-acceptor)
(require view-character-model)
(require view-armor-picker)
(require view-augment-picker)
(require view-faction-picker)
(require view-weapon-picker)
(require view-name-generator)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class com.jme3.app Application SimpleApplication))
(import (class com.jme3.app.state AbstractAppState AppStateManager))
(import (class com.jme3.font BitmapFont))
(import (class com.jme3.input InputManager MouseInput))
(import (class com.jme3.input.controls MouseAxisTrigger MouseButtonTrigger))
(import (class com.jme3.math ColorRGBA Vector2f Vector3f Vector4f))
(import (class com.jme3.network Client Network))
(import (class com.jme3.network.base DefaultClient))
(import (class com.jme3.network.serializing Serializer))
(import (class com.jme3.scene CameraNode Node Spatial))
(import (class java.net ConnectException))
(import (class tonegod.gui.controls.buttons ButtonAdapter))
(import (class tonegod.gui.controls.text Label))
(import (class tonegod.gui.controls.windows Window))
(import (class tonegod.gui.core Screen))

;;; ---------------------------------------------------------------------
;;; the client character-creator AppState class
;;; ---------------------------------------------------------------------

;;; CLASS CreateCharacterState
;;; ---------------------------------------------------------------------

(define-simple-class CreateCharacterState (FabricClientState)
  ;; slots
  (name-picker init-form: #!null)
  (name-generator init-form: #!null)
  (character::FabricCharacter init-form: #!null)
  (character-nameplate::Label init-form: #!null)
  (faction-nameplate init-form: #!null)
  (faction-picker init-form: #!null)
  (weapon-picker init-form: #!null)
  (armor-picker init-form: #!null)
  (augment-picker init-form: #!null)
  (character-acceptor init-form: #!null)
  (abjurers-button init-form: #!null)
  (caretakers-button init-form: #!null)
  (rogues-button init-form: #!null)
  (initialized? init: #f)
  ;; methods
  ((cleanup) (%create-character-state-cleanup (this)))
  ((isEnabled) (%create-character-state-enabled? (this)))
  ((stateAttached state-manager::AppStateManager)
   (%create-character-state-attached (this) state-manager))
  ((stateDetached state-manager::AppStateManager)
   (%create-character-state-detached (this) state-manager))
  ((handleAnalogEvent name value tpf)
   (create-character-state-handle-analog-event (this) name value tpf))
  ((handleActionEvent name key-pressed? tpf)
   (create-character-state-handle-action-event (this) name key-pressed? tpf))
  ;; init
  ((initialize) (%create-character-state-initialize (this)))
  ((isInitialized) initialized?))

(define (%create-character-state-cleanup state::CreateCharacterState)
  (format #t "~%%create-character-state-cleanup called"))

(define (%create-character-state-initialize state::CreateCharacterState)
  (format #t "~%%create-character-state-initialize called"))

(define (%create-character-state-enabled? state::CreateCharacterState) #t)

(define (%create-character-state-attached state::CreateCharacterState manager::AppStateManager)
  (let ((client::Application state:client))
    (prepare-to-attach-create-character-state state client)
    (did-attach-create-character-state state manager)))

(define (%create-character-state-detached state::CreateCharacterState manager::AppStateManager)
  (did-detach-create-character-state state manager))

(define (make-create-character-state client::Application character::FabricCharacter)
  (let ((state (CreateCharacterState)))
    (set! state:client client)
    (set! state:character character)
    state))

;;; ---------------------------------------------------------------------
;;; event-handling
;;; ---------------------------------------------------------------------

;;; (create-character-state-handle-analog-event state name value tpf)
;;; ---------------------------------------------------------------------
;;; handle mouse movements and other continuous events

(define (create-character-state-handle-analog-event state::CreateCharacterState name value tpf)
  (let* ((client::FabricClient state:client)
         (pchar::FabricCharacter state:character)
         (model state:character:model)
         (right-button-down? client:right-button?))
    (on-analog (name)
               ("KeyD" -> (rotate-node-right! model (* 1 tpf)))
               ("KeyA" -> (rotate-node-left! model (* 1 tpf)))
               ("KeyW" -> (rotate-node-up! model (* 0.5 tpf)))
               ("KeyS" -> (rotate-node-down! model (* 0.5 tpf)))
               ("MouseDragLeft" -> (when right-button-down?(rotate-node-right! model value)))
               ("MouseDragRight" -> (when right-button-down?(rotate-node-left! model value)))
               ("MouseDragUp" -> (when right-button-down?(rotate-node-up! model value)))
               ("MouseDragDown" -> (when right-button-down?(rotate-node-down! model value))))))

;;; (create-character-state-handle-action-event state name key-pressed? tpf)
;;; ---------------------------------------------------------------------
;;; handle keypresses, mouse clicks, and other discrete events

(define (create-character-state-handle-action-event state::CreateCharacterState name key-pressed? tpf)
  (let ((client::FabricClient state:client))
    (on-action (name)
               ("MouseButtonLeft" -> (set! client:left-button? key-pressed?))
               ("MouseButtonRight" -> (set! client:right-button? key-pressed?)))))

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


(define (compute-name-generator-rect screen::Screen)
  (let* ((weapon-picker-rect (compute-weapon-picker-rect screen))
         (screen-width (*:getWidth screen))
         (screen-height (*:getHeight screen))
         (generator-width 256)
         (generator-height 220)
         (generator-top (- screen-height
                           (+ generator-height 8)))
         (generator-left 8))
    (make-rectangle generator-left
                    generator-top
                    generator-width
                    generator-height)))

;;; attach and detach

(define (prepare-to-attach-create-character-state state::CreateCharacterState client::FabricClient)
  (unless state:initialized?
    (let* ((screen::Screen client:screen)
           (gui-node::Node client:guiNode)
           (Align BitmapFont:Align)
           (camera::com.jme3.renderer.Camera (*:getCamera client)))
      (set! state:faction-picker (make-faction-picker state screen))
      (set! state:weapon-picker (make-weapon-picker state screen))
      (set! state:armor-picker (make-armor-picker state screen))
      (set! state:augment-picker (make-augment-picker state screen))
      (set! state:name-generator (make-name-generator screen state state:character:name))
      (set! state:character-acceptor (make-character-acceptor screen))
      (set! state:faction-nameplate (make-faction-nameplate screen))
      (set! state:character-nameplate (make-character-nameplate screen))
      (set! state:character:model state:character:model)
      (set! state:initialized? #t))))

(define (did-attach-create-character-state state::CreateCharacterState mgr::AppStateManager)
  (when state:initialized?
    (let* ((client::FabricClient state:client)
           (screen::Screen client:screen)
           (gui-node::Node client:guiNode)
           (model state:character:model)
           (character::FabricCharacter state:character)
           (faction character:faction)
           (fname character:name)
           (root::Node (*:getRootNode client))
           (lit-color (faction-lit-color faction))
           (dim-color (faction-dim-color faction)))
      (*:enqueue client
                 (runnable (lambda ()
                             (*:addElement screen state:faction-nameplate)
                             (*:addElement screen state:character-nameplate)
                             (*:addElement screen state:faction-picker)
                             (*:addElement screen state:weapon-picker)
                             (*:addElement screen state:armor-picker)
                             (*:addElement screen state:augment-picker)
                             (*:addElement screen state:name-generator)
                             (*:addElement screen state:character-acceptor)
                             (*:setLocalTranslation (as Node model) 0.0 0.0 -30.0)
                             (*:attachChild root model)
                             (*:setText state:character-nameplate (fabric-name->string fname))
                             (recolor-character-model! character lit-color dim-color)
                             (*:addControl gui-node screen)
                             (setup-inputs client)))))))

(define (did-detach-create-character-state state::CreateCharacterState mgr::AppStateManager)
  (when state:initialized?
    (let* ((client::FabricClient state:client)
           (screen::Screen client:screen)
           (gui-node::Node client:guiNode)
           (model state:character:model))
      (*:enqueue client
                 (runnable (lambda ()
                             (let ((root::Node client:rootNode))
                               (teardown-inputs client)
                               (*:removeElement screen state:faction-nameplate)
                               (*:removeElement screen state:character-nameplate)
                               (*:removeElement screen state:faction-picker)
                               (*:removeElement screen state:weapon-picker)
                               (*:removeElement screen state:armor-picker)
                               (*:removeElement screen state:augment-picker)
                               (*:removeElement screen state:name-generator)
                               (*:removeElement screen state:character-acceptor)
                               (*:detachChild root model)
                               (*:removeControl gui-node screen))))))))
