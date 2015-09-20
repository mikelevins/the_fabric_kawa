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
(require util-java)
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

(import-as AbstractAppState com.jme3.app.state.AbstractAppState)
(import-as AppStateManager com.jme3.app.state.AppStateManager)
(import-as Application com.jme3.app.Application)
(import-as BitmapFont com.jme3.font.BitmapFont)
(import-as ButtonAdapter tonegod.gui.controls.buttons.ButtonAdapter)
(import-as Client com.jme3.network.Client)
(import-as ColorRGBA com.jme3.math.ColorRGBA)
(import-as ConnectException java.net.ConnectException)
(import-as DefaultClient com.jme3.network.base.DefaultClient)
(import-as Label tonegod.gui.controls.text.Label)
(import-as MouseAxisTrigger com.jme3.input.controls.MouseAxisTrigger)
(import-as MouseButtonTrigger com.jme3.input.controls.MouseButtonTrigger)
(import-as MouseInput com.jme3.input.MouseInput)
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
  (name-picker init-form: #!null)
  (name-generator init-form: #!null)
  (character::FabricCharacter init-form: #!null)
  (character-nameplate::Label init-form: #!null)
  (character-model init-form: #!null)
  (faction-nameplate init-form: #!null)
  (faction-picker init-form: #!null)
  (weapon init-form: #!null)
  (weapon-picker init-form: #!null)
  (armor init-form: #!null)
  (armor-picker init-form: #!null)
  (augment init-form: #!null)
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

;;; (setup-inputs app::FabricClient)
;;; ---------------------------------------------------------------------
;;; establishes the event handlers that translate keypresses and
;;; mouse movements into movements of the player's node and camera

(define (setup-inputs app::FabricClient)
  ;; set up the player's controls
  (let* ((input-manager (*:getInputManager app)))
    (route-keys (input-manager)
                ((MouseButtonTrigger MouseInput:BUTTON_LEFT) ->  "leftButton")
                ((MouseButtonTrigger MouseInput:BUTTON_RIGHT) -> "rightButton")
                ((MouseAxisTrigger 0 #f) -> "mouseRotateLeft")
                ((MouseAxisTrigger 0 #t) -> "mouseRotateRight")
                ((MouseAxisTrigger 1 #f) -> "mouseRotateUp")
                ((MouseAxisTrigger 1 #t) -> "mouseRotateDown"))
    ;; set up the event listener
    (*:addListener input-manager app
                   ;; motion controls
                   "leftButton" "rightButton"
                   "mouseRotateRight" "mouseRotateLeft" "mouseRotateUp" "mouseRotateDown")))


;;; (teardown-inputs app::FabricClient)
;;; ---------------------------------------------------------------------
;;; removes the event handlers that translate keypresses and
;;; mouse movements into movements of the player's node and camera

(define (teardown-inputs app::FabricClient)
  ;; set up the player's controls
  (let* ((input-manager (*:getInputManager app)))
    (unroute-keys (input-manager)
                  "leftButton"
                  "mouseRotateDown"
                  "mouseRotateLeft"
                  "mouseRotateRight"
                  "mouseRotateUp")
    ;; set up the event listener
    (*:removeListener input-manager app)))

;;; (create-character-state-handle-analog-event state name value tpf)
;;; ---------------------------------------------------------------------
;;; handle mouse movements and other continuous events

(define (create-character-state-handle-analog-event state::CreateCharacterState name value tpf)
  (let* ((client::FabricClient state:client)
         (pchar::FabricCharacter state:character)
         (model state:character-model)
         (right-button-down? client:right-button?))
    (on-analog (name)
               ("rotateRight" -> (rotate-node-right! model (* 0.25 tpf)))
               ("mouseRotateRight" -> (when right-button-down?
                                        (rotate-node-right! model value)))
               ("rotateLeft" -> (rotate-node-left! model (* 0.25 tpf)))
               ("mouseRotateLeft" -> (when right-button-down?
                                       (rotate-node-left! model value)))
               ("rotateUp" -> (rotate-node-up! model (* 0.125 tpf)))
               ("mouseRotateUp" -> (when right-button-down?
                                     (rotate-node-up! model value)))
               ("rotateDown" -> (rotate-node-down! model (* 0.125 tpf)))
               ("mouseRotateDown" -> (when right-button-down?
                                       (rotate-node-down! model value))))))

;;; (create-character-state-handle-action-event state name key-pressed? tpf)
;;; ---------------------------------------------------------------------
;;; handle keypresses, mouse clicks, and other discrete events

(define (create-character-state-handle-action-event state::CreateCharacterState name key-pressed? tpf)
  (let ((client::FabricClient state:client))
    (on-action (name)
               ("leftButton" -> (set! client:left-button? key-pressed?))
               ("rightButton" -> (set! client:right-button? key-pressed?)))))

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
           (faction-nameplate::Label (make-faction-nameplate screen))
           (character-nameplate::Label (make-character-nameplate screen))
           (faction-picker::Window (make-faction-picker state screen))
           (weapon-picker::Window (make-weapon-picker state screen))
           (armor-picker::Window (make-armor-picker state screen))
           (augment-picker::Window (make-augment-picker state screen))
           ;;(name-picker::Window (make-name-picker screen))
           (name-generator::Window (make-name-generator screen state state:character:name))
           (character-acceptor::Window (make-character-acceptor screen))
           (character-model state:character:model))
      (set! state:faction-picker faction-picker)
      (set! state:weapon-picker weapon-picker)
      (set! state:armor-picker armor-picker)
      (set! state:augment-picker augment-picker)
      ;;(set! state:name-picker name-picker)
      (set! state:name-generator name-generator)
      (set! state:character-acceptor character-acceptor)
      (set! state:faction-nameplate faction-nameplate)
      (set! state:character-nameplate character-nameplate)
      (set! state:character-model character-model)
      (setup-inputs client)
      (set! state:initialized? #t))))

(define (did-attach-create-character-state state::CreateCharacterState mgr::AppStateManager)
  (when state:initialized?
    (let* ((client::FabricClient state:client)
           (screen::Screen client:screen)
           (gui-node::Node client:guiNode)
           (model state:character-model)
           (character::FabricCharacter state:character)
           (faction character:faction)
           (fname character:name))
      (*:enqueue client
                 (runnable (lambda ()
                             (let ((root::Node (*:getRootNode client))
                                   (lit-color (if (eqv? #!null faction)
                                                  (default-glow-color)
                                                  (case faction
                                                    ((caretakers)(bright-caretakers-color))
                                                    ((rogues)(bright-rogues-color))
                                                    ((abjurers)(bright-abjurers-color))
                                                    (else (default-glow-color)))))
                                   (dim-color (if (eqv? #!null faction)
                                                  (default-character-color)
                                                  (case faction
                                                    ((caretakers)(dim-caretakers-color))
                                                    ((rogues)(dim-rogues-color))
                                                    ((abjurers)(dim-abjurers-color))
                                                    (else (default-character-color))))))
                               (*:addElement screen state:faction-nameplate)
                               (*:addElement screen state:character-nameplate)
                               (*:addElement screen state:faction-picker)
                               (*:addElement screen state:weapon-picker)
                               (*:addElement screen state:armor-picker)
                               (*:addElement screen state:augment-picker)
                               ;;(*:addElement screen state:name-picker)
                               (*:addElement screen state:name-generator)
                               (*:addElement screen state:character-acceptor)
                               (*:setLocalTranslation (as Node model) 0.0 0.0 -5.0)
                               (*:attachChild root model)
                               (*:setText state:character-nameplate (fabric-name->string fname))
                               (recolor-character-model! character lit-color dim-color)
                               (*:addControl gui-node screen))))))))

(define (did-detach-create-character-state state::CreateCharacterState mgr::AppStateManager)
  (when state:initialized?
    (let* ((client::FabricClient state:client)
           (screen::Screen client:screen)
           (gui-node::Node client:guiNode)
           (model state:character-model))
      (*:enqueue client
                 (runnable (lambda ()
                             (let ((root::Node client:rootNode))
                               (*:removeElement screen state:faction-nameplate)
                               (*:removeElement screen state:character-nameplate)
                               (*:removeElement screen state:faction-picker)
                               (*:removeElement screen state:weapon-picker)
                               (*:removeElement screen state:armor-picker)
                               (*:removeElement screen state:augment-picker)
                               ;;(*:removeElement screen state:name-picker)
                               (*:removeElement screen state:name-generator)
                               (*:removeElement screen state:character-acceptor)
                               (*:detachChild root model)
                               (teardown-inputs client)
                               (*:removeControl gui-node screen))))))))
