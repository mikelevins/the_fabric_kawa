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

(require "util-error.scm")
(require "util-java.scm")
(require "util-lists.scm")
(require "data-nodes.scm")
(require "view-skybox.scm")
(require "view-celestial-body.scm")
(require "syntax-events.scm")
(require "model-character.scm")
(require "client-class.scm")
(require "client-state.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as AppStateManager com.jme3.app.state.AppStateManager)
(import-as Application com.jme3.app.Application)
(import-as BitmapFont com.jme3.font.BitmapFont)
(import-as CameraControl com.jme3.scene.control.CameraControl)
(import-as CameraNode com.jme3.scene.CameraNode)
(import-as Geometry com.jme3.scene.Geometry)
(import-as KeyInput com.jme3.input.KeyInput)
(import-as KeyTrigger com.jme3.input.controls.KeyTrigger)
(import-as Label tonegod.gui.controls.text.Label)
(import-as MouseAxisTrigger com.jme3.input.controls.MouseAxisTrigger)
(import-as MouseButtonTrigger com.jme3.input.controls.MouseButtonTrigger)
(import-as MouseInput com.jme3.input.MouseInput)
(import-as Node com.jme3.scene.Node)
(import-as Panel tonegod.gui.controls.windows.Panel)
(import-as PI com.jme3.math.FastMath:PI)
(import-as Quaternion com.jme3.math.Quaternion)
(import-as Screen tonegod.gui.core.Screen)
(import-as Spatial com.jme3.scene.Spatial)
(import-as Vector2f com.jme3.math.Vector2f)
(import-as Vector3f com.jme3.math.Vector3f)
(import-as Window tonegod.gui.controls.windows.Window)

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

;;; (setup-inputs app::FabricClient)
;;; ---------------------------------------------------------------------
;;; establishes the event handlers that translate keypresses and
;;; mouse movements into movements of the player's node and camera

(define (setup-inputs app::FabricClient)
  ;; set up the player's controls
  (let* ((key-input ::KeyInput (*:getKeyInput app))
         (input-manager (*:getInputManager app)))
    (route-keys (input-manager)
                ((KeyTrigger key-input:KEY_UP) -> "moveForward")
                ((KeyTrigger key-input:KEY_W) ->  "moveForward")
                ((MouseButtonTrigger MouseInput:BUTTON_LEFT) ->  "maybeMoveForward")
                ((MouseButtonTrigger MouseInput:BUTTON_LEFT) ->  "leftButton")
                ((MouseButtonTrigger MouseInput:BUTTON_RIGHT) -> "rightButton")
                ((KeyTrigger key-input:KEY_RIGHT) -> "moveRight")
                ((KeyTrigger key-input:KEY_D) -> "moveRight")
                ((MouseAxisTrigger 0 #f) -> "mouseRotateRight")
                ((KeyTrigger key-input:KEY_LEFT) -> "moveLeft")
                ((KeyTrigger key-input:KEY_A) -> "moveLeft")
                ((MouseAxisTrigger 0 #t) -> "mouseRotateLeft")
                ((MouseAxisTrigger 1 #f) -> "mouseRotateUp")
                ((KeyTrigger key-input:KEY_DOWN) -> "moveBackward")
                ((KeyTrigger key-input:KEY_S) -> "moveBackward")
                ((MouseAxisTrigger 1 #t) -> "mouseRotateDown")
                ;; text inputs
                ((KeyTrigger key-input:KEY_SPACE) -> "SPACE")
                ((KeyTrigger key-input:KEY_A) -> "KEY_A"))
    ;; set up the event listener
    (*:addListener input-manager app
                   ;; motion controls
                   "moveForward" "maybeMoveForward" "moveBackward" "moveRight" "moveLeft"
                   "leftButton" "rightButton" "rotateRight" "rotateLeft" "rotateUp" "rotateDown"
                   "mouseRotateRight" "mouseRotateLeft" "mouseRotateUp" "mouseRotateDown"
                   ;; chat input
                   "SPACE" "KEY_A")))


;;; (teardown-inputs app::FabricClient)
;;; ---------------------------------------------------------------------
;;; removes the event handlers that translate keypresses and
;;; mouse movements into movements of the player's node and camera

(define (teardown-inputs app::FabricClient)
  ;; set up the player's controls
  (let* ((key-input ::KeyInput (*:getKeyInput app))
         (input-manager (*:getInputManager app)))
    (unroute-keys (input-manager)
                  "leftButton"
                  "maybeMoveForward"
                  "mouseRotateDown"
                  "mouseRotateLeft"
                  "mouseRotateRight"
                  "mouseRotateUp"
                  "moveBackward"
                  "moveForward"
                  "moveLeft"
                  "moveRight"
                  "rightButton"
                  ;; text inputs
                  "SPACE"
                  "KEY_A")
    ;; set up the event listener
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
         (node pchar:node)
         (right-button-down? client:right-button?))
    (on-analog (name)
               ("moveForward" -> (move-node-forward! client node (* speed tpf)))
               ("maybeMoveForward" -> (when right-button-down?
                                        (move-node-forward! client node (* speed tpf))))
               ("moveBackward" -> (move-node-backward! client node (* 0.6 speed tpf)))
               ("moveRight" -> (move-node-right! client node (* speed tpf)))
               ("moveLeft" -> (move-node-left! client node (* speed tpf)))
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
  (let* ((pchar::FabricCharacter state:player-character)
         (pnode::Node pchar:node)
         (rotation (Quaternion))
         (pitch-axis (Vector3f 1 0 0)))
    (*:setLocalTranslation pnode 0.0 20000.0 0.0)
    ;; PI/4 radians points us right at the center
    (*:fromAngleAxis rotation (/ PI 4) pitch-axis)
    (*:setLocalRotation pnode rotation)))

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
           (pnode::Node pchar:node))
      (*:setControlDir cam-node CameraControl:ControlDirection:SpatialToCamera)
      (*:setFrustumFar camera 80000)
      (*:setLocalTranslation cam-node (Vector3f 0 30 -30))
      (*:lookAt cam-node (*:getLocalTranslation pnode) Vector3f:UNIT_Y)
      (*:attachChild pnode cam-node)
      (set! state:celestial-body body)
      (set! state:sky sky)
      (*:setEnabled (*:getFlyByCamera client) #f)
      (setup-inputs client)
      (set! state:initialized? #t))))

(define (did-attach-play-state state::PlayState mgr::AppStateManager)
  (when state:initialized?
    (let* ((client::FabricClient state:client)
           (screen::Screen client:screen)
           (gui-node::Node client:guiNode)
           (pchar::FabricCharacter state:player-character)
           (pnode::Node pchar:node))
      (*:enqueue client
                 (runnable (lambda ()
                             (let* ((root::Node client:rootNode))
                               (reset-play-state! state)
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
                               (teardown-inputs client))))))))


