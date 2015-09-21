;;;; ***********************************************************************
;;;;
;;;; Name:          state-play.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       fabric AppStates
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************
(format #t "~%loading state-play.scm")

(module-export
 PlayState
 make-play-state
 reset-play-state!)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require util-error)
(require util-lists)
(require data-nodes)
(require view-skybox)
(require view-celestial-body)
(require syntax-events)
(require model-character)
(require client-class)
(require client-state)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (only (com jme3 math FastMath) PI))

(import (class com.jme3.app Application))
(import (class com.jme3.app.state AppStateManager))
(import (class com.jme3.font BitmapFont))
(import (class com.jme3.input InputManager KeyInput MouseInput))
(import (class com.jme3.input.controls KeyTrigger MouseAxisTrigger MouseButtonTrigger))
(import (class com.jme3.math Quaternion Vector2f Vector3f))
(import (class com.jme3.scene CameraNode Geometry Node Spatial))
(import (class com.jme3.scene.control CameraControl))
(import (class tonegod.gui.controls.text Label))
(import (class tonegod.gui.controls.windows Panel Window))
(import (class tonegod.gui.core Screen))

;;; ---------------------------------------------------------------------
;;; the client play AppState class
;;; ---------------------------------------------------------------------

;;; CLASS PlayState
;;; ---------------------------------------------------------------------

(define-simple-class PlayState (FabricClientState)
  ;; slots
  (player-character init: #!null)
  (node-name init: #f)
  (celestial-body init: #!null)
  (sky init: #f)
  (speed type: float init-form: 3000.0)
  (initialized? init: #f)
  ;; methods
  ((cleanup) (%play-state-cleanup (this)))
  ((isEnabled) (%play-state-enabled? (this)))
  ((stateAttached state-manager::AppStateManager)
   (%play-state-attached (this) state-manager))
  ((stateDetached state-manager::AppStateManager)
   (%play-state-detached (this) state-manager))
  ((handleAnalogEvent name value tpf)
   (play-state-handle-analog-event (this) name value tpf))
  ((handleActionEvent name key-pressed? tpf)
   (play-state-handle-action-event (this) name key-pressed? tpf))
  ((initialize) (%play-state-initialize (this)))
  ((isInitialized) (%play-state-initialized? (this))))

;;; ---------------------------------------------------------------------
;;; PlayState setup and teardown
;;; ---------------------------------------------------------------------

(define (%play-state-cleanup state::PlayState)
  (format #t "~%%play-state-cleanup called"))

(define (%play-state-initialize state::PlayState)
  (format #t "~%%play-state-initialize called"))

(define (%play-state-enabled? state::PlayState) #t)

(define (%play-state-initialized? state::PlayState) #t)

(define (%play-state-attached state::PlayState manager::AppStateManager)
  (let ((client::Application state:client))
    (prepare-to-attach-play-state state client)
    (did-attach-play-state state manager)))

(define (%play-state-detached state::PlayState manager::AppStateManager)
  (did-detach-play-state state manager))

;;; ---------------------------------------------------------------------
;;; construction
;;; ---------------------------------------------------------------------

(define (make-play-state client::Application character::FabricCharacter node-name)
  (let ((state (PlayState)))
    (set! state:client client)
    (set! state:node-name node-name)
    (set! state:player-character character)
    state))


;;; ---------------------------------------------------------------------
;;; input-handling
;;; ---------------------------------------------------------------------

;;; (state-play-setup-inputs app::FabricClient)
;;; ---------------------------------------------------------------------
;;; establishes the event handlers that translate keypresses and
;;; mouse movements into movements of the player's node and camera

(define (state-play-setup-inputs app::FabricClient)
  ;; set up the player's controls
  (let* ((key-input::KeyInput (*:getKeyInput app))
         (input-manager::InputManager (*:getInputManager app)))
    (*:addMapping input-manager "moveForward" (KeyTrigger key-input:KEY_W))
    (*:addMapping input-manager "maybeMoveForward" (MouseButtonTrigger MouseInput:BUTTON_LEFT))
    (*:addMapping input-manager "leftButton" (MouseButtonTrigger MouseInput:BUTTON_LEFT))
    (*:addMapping input-manager "rightButton" (MouseButtonTrigger MouseInput:BUTTON_RIGHT))
    (*:addMapping input-manager "moveRight" (KeyTrigger key-input:KEY_D))
    (*:addMapping input-manager "mouseRotateRight" (MouseAxisTrigger 0 #f))
    (*:addMapping input-manager "moveLeft" (KeyTrigger key-input:KEY_A))
    (*:addMapping input-manager "mouseRotateLeft" (MouseAxisTrigger 0 #t))
    (*:addMapping input-manager "mouseRotateUp" (MouseAxisTrigger 1 #f))
    (*:addMapping input-manager "moveBackward" (KeyTrigger key-input:KEY_S))
    (*:addMapping input-manager "mouseRotateDown" (MouseAxisTrigger 1 #t))
    (*:addMapping input-manager "moveUp" (KeyTrigger key-input:KEY_SPACE))
    (*:addMapping input-manager "moveDown" (KeyTrigger key-input:KEY_X))
    ;; set up the event listener
    (*:addListener input-manager app
                   ;; motion controls
                   "moveForward" "maybeMoveForward" "moveBackward" "moveRight" "moveLeft"
                   "moveUp" "moveDown"
                   "leftButton" "rightButton" "rotateRight" "rotateLeft" "rotateUp" "rotateDown"
                   "mouseRotateRight" "mouseRotateLeft" "mouseRotateUp" "mouseRotateDown"
                   ;; chat input
                   "SPACE" "KEY_A")))


;;; (state-play-teardown-inputs app::FabricClient)
;;; ---------------------------------------------------------------------
;;; removes the event handlers that translate keypresses and
;;; mouse movements into movements of the player's node and camera

(define (state-play-teardown-inputs app::FabricClient)
  ;; set up the player's controls
  (let* ((key-input::KeyInput (*:getKeyInput app))
         (input-manager::InputManager (*:getInputManager app)))
    (*:deleteTrigger input-manager "moveForward" (KeyTrigger key-input:KEY_UP))
    (*:deleteTrigger input-manager "moveForward" (KeyTrigger key-input:KEY_W))
    (*:deleteTrigger input-manager "maybeMoveForward" (MouseButtonTrigger MouseInput:BUTTON_LEFT))
    (*:deleteTrigger input-manager "leftButton" (MouseButtonTrigger MouseInput:BUTTON_LEFT))
    (*:deleteTrigger input-manager "rightButton" (MouseButtonTrigger MouseInput:BUTTON_RIGHT))
    (*:deleteTrigger input-manager "moveRight" (KeyTrigger key-input:KEY_D))
    (*:deleteTrigger input-manager "mouseRotateRight" (MouseAxisTrigger 0 #f))
    (*:deleteTrigger input-manager "moveLeft" (KeyTrigger key-input:KEY_A))
    (*:deleteTrigger input-manager "mouseRotateLeft" (MouseAxisTrigger 0 #t))
    (*:deleteTrigger input-manager "mouseRotateUp" (MouseAxisTrigger 1 #f))
    (*:deleteTrigger input-manager "moveBackward" (KeyTrigger key-input:KEY_S))
    (*:deleteTrigger input-manager "mouseRotateDown" (MouseAxisTrigger 1 #t))
    (*:deleteTrigger input-manager "moveUp" (KeyTrigger key-input:KEY_SPACE))
    (*:deleteTrigger input-manager "moveDown" (KeyTrigger key-input:KEY_X))
    (*:removeListener input-manager app)))

;;; ---------------------------------------------------------------------
;;; event-handling
;;; ---------------------------------------------------------------------

;;; (play-state-handle-analog-event state name value tpf)
;;; ---------------------------------------------------------------------
;;; handle mouse movements and other continuous events

(define (play-state-handle-analog-event state::PlayState name value tpf)
  (let* ((client::FabricClient state:client)
         (speed state:speed)
         (pchar::FabricCharacter state:player-character)
         (node pchar:model)
         (right-button-down? client:right-button?))
    (on-analog (name)
               ("moveForward" -> (move-node-forward! client node (* speed tpf)))
               ("maybeMoveForward" -> (when right-button-down?
                                        (move-node-forward! client node (* speed tpf))))
               ("moveBackward" -> (move-node-backward! client node (* 0.6 speed tpf)))
               ("moveRight" -> (move-node-right! client node (* speed tpf)))
               ("moveLeft" -> (move-node-left! client node (* speed tpf)))
               ("moveUp" -> (move-node-up! client node (* speed tpf)))
               ("moveDown" -> (move-node-down! client node (* speed tpf)))
               ("rotateRight" -> (rotate-node-right! node (* 0.25 tpf)))
               ("mouseRotateRight" -> (when right-button-down?
                                        (rotate-node-right! node value)))
               ("rotateLeft" -> (rotate-node-left! node (* 0.25 tpf)))
               ("mouseRotateLeft" -> (when right-button-down?
                                       (rotate-node-left! node value)))
               ("rotateUp" -> (rotate-node-up! node (* 0.125 tpf)))
               ("mouseRotateUp" -> (when right-button-down?
                                     (rotate-node-up! node value)))
               ("rotateDown" -> (rotate-node-down! node (* 0.125 tpf)))
               ("mouseRotateDown" -> (when right-button-down?
                                       (rotate-node-down! node value))))))

;;; (play-state-handle-action-event state name key-pressed? tpf)
;;; ---------------------------------------------------------------------
;;; handle keypresses, mouse clicks, and other discrete events

(define (play-state-handle-action-event state::PlayState name key-pressed? tpf)
  (let ((client::FabricClient state:client))
    (on-action (name)
               ("leftButton" -> (set! client:left-button? key-pressed?))
               ("rightButton" -> (set! client:right-button? key-pressed?)))))

;;; ---------------------------------------------------------------------
;;; PlayState functions
;;; ---------------------------------------------------------------------

(define (->texture-name name-text)
  (string-append name-text ".jpg"))

;;; (reset-play-state! state::PlayState)
;;; ---------------------------------------------------------------------
;;; reset the play scene to its default starting state, with the
;;; default camera position and orientation

(define (reset-play-state! state::PlayState)
  (let* ((client::FabricClient state:client)
         (pchar::FabricCharacter state:player-character)
         (pnode::Node pchar:model)
         (rotation (Quaternion))
         (pitch-axis (Vector3f 1 0 0)))
    (*:setLocalTranslation pnode 0.0 0.0 -25000.0)
    (*:fromAngleAxis rotation (* -1 (/ PI 4)) pitch-axis)
    (*:setLocalRotation pnode rotation)
    (normalize-camera! client)))

(define (prepare-to-attach-play-state state::PlayState client::FabricClient)
  (unless state:initialized?
    (let* ((screen::Screen client:screen)
           (gui-node::Node client:guiNode)
           (Align BitmapFont:Align)
           (name-text state:node-name)
           (texture-name (->texture-name name-text))
           (body (make-celestial-body texture-name))
           (sky::Spatial (make-sky-box))
           (camera::com.jme3.renderer.Camera (*:getCamera client))
           (cam-node (CameraNode "camera" camera))
           (pchar::FabricCharacter state:player-character)
           (pnode::Node pchar:model))
      (*:attachChild pnode cam-node)
      (*:setControlDir cam-node CameraControl:ControlDirection:SpatialToCamera)
      (*:setFrustumFar camera 80000)
      (*:setLocalTranslation cam-node (Vector3f 0 30 -30))
      (*:lookAt cam-node (*:getLocalTranslation pnode) Vector3f:UNIT_Y)
      (set! state:celestial-body body)
      (set! state:sky sky)
      (*:setEnabled (*:getFlyByCamera client) #f)
      (state-play-setup-inputs client)
      (reset-play-state! state)
      (set! state:initialized? #t))))

(define (did-attach-play-state state::PlayState mgr::AppStateManager)
  (when state:initialized?
    (let* ((client::FabricClient state:client)
           (screen::Screen client:screen)
           (gui-node::Node client:guiNode)
           (pchar::FabricCharacter state:player-character)
           (pnode::Node pchar:model))
      (*:enqueue client
                 (runnable (lambda ()
                             (let* ((root::Node client:rootNode))
                               (*:attachChild root state:sky)
                               (*:attachChild root state:celestial-body)
                               (*:attachChild root pnode)
                               (*:addControl gui-node screen))))))))

(define (did-detach-play-state state::PlayState mgr::AppStateManager)
  (when state:initialized?
    (let* ((client::FabricClient state:client)
           (screen::Screen client:screen)
           (gui-node::Node client:guiNode))
      (*:enqueue client
                 (runnable (lambda ()
                             (let ((root::Node client:rootNode)
                                   (sky::Spatial state:sky)
                                   (body state:celestial-body))
                               (*:detachChild root sky)
                               (*:detachChild root body)
                               (*:removeControl gui-node screen)
                               (state-play-teardown-inputs client))))))))


