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
 make-play-state)

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
(require "client-class.scm")
(require "client-state.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as AppStateManager com.jme3.app.state.AppStateManager)
(import-as Application com.jme3.app.Application)
(import-as BitmapFont com.jme3.font.BitmapFont)
(import-as Geometry com.jme3.scene.Geometry)
(import-as KeyInput com.jme3.input.KeyInput)
(import-as KeyTrigger com.jme3.input.controls.KeyTrigger)
(import-as Label tonegod.gui.controls.text.Label)
(import-as MouseAxisTrigger com.jme3.input.controls.MouseAxisTrigger)
(import-as MouseButtonTrigger com.jme3.input.controls.MouseButtonTrigger)
(import-as MouseInput com.jme3.input.MouseInput)
(import-as Node com.jme3.scene.Node)
(import-as Panel tonegod.gui.controls.windows.Panel)
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
  (node-name init: #f)
  ((getNodeName) node-name)
  ((setNodeName new-name) (set! node-name new-name))
  (celestial-body init: #!null)
  ((getCelestialBody) celestial-body)
  ((setCelestialBody new-body)(set! celestial-body new-body))
  (sky init: #f)
  ((getSky) sky)
  ((setSky new-sky) (set! sky new-sky))
  (initialized? init: #f)
  ((getInitialized) initialized?)
  ((setInitialized newstate) (set! initialized? newstate))
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

(define (%play-state-cleanup state::PlayState)
  (format #t "~%%play-state-cleanup called"))

(define (%play-state-initialize state::PlayState)
  (format #t "~%%play-state-initialize called"))

(define (%play-state-enabled? state::PlayState) #t)

(define (%play-state-initialized? state::PlayState) #t)

(define (%play-state-attached state::PlayState manager::AppStateManager)
  (let ((client::Application (*:getClient state)))
    (prepare-to-attach-play-state state client)
    (did-attach-play-state state manager)))

(define (%play-state-detached state::PlayState manager::AppStateManager)
  (did-detach-play-state state manager))

(define (make-play-state client::Application node-name)
  (let ((state (PlayState)))
    (*:setClient state client)
    (*:setNodeName state node-name)
    state))

;;; ---------------------------------------------------------------------
;;; player movement
;;; ---------------------------------------------------------------------


;;; (normalize-camera! app :: FabricClient)
;;; ---------------------------------------------------------------------
;;; orients the camera to where the player's character node is facing

(define (normalize-camera! app :: FabricClient)
  (let ((dir :: Vector3f (*:getCameraDirection app)))
    (*:normalizeLocal dir)))


;;; (move-player!  app :: FabricClient node :: Node amount :: float invert?)
;;; ---------------------------------------------------------------------
;;; moves the player's node _node_  an distance along an arbitrary vector.
;;; used by more specific move- functions like move-player-forward!

(define (move-player!  app :: FabricClient node :: Node amount :: float invert?)
  (let ((dir :: Vector3f (*:getDirection app))
        (sign (if invert? -1 1)))
    (*:multLocal dir (* sign amount))
    (*:move node dir)))


;;; (move-player-forward! app :: FabricClient node :: Node amount :: float)
;;; ---------------------------------------------------------------------
;;; moves _node_ forward a distance of _amount_

(define (move-player-forward! app :: FabricClient node :: Node amount :: float)
  (normalize-camera! app)
  (*:setDirection app (*:getCameraDirection app))
  (move-player! app node amount #f))


;;; (move-player-backward! app :: FabricClient node :: Node amount :: float)
;;; ---------------------------------------------------------------------
;;; moves _node_ backward a distance of _amount_

(define (move-player-backward! app :: FabricClient node :: Node amount :: float)
  (normalize-camera! app)
  (*:setDirection app (*:getCameraDirection app))
  (move-player! app node amount #t))


;;; (move-player-left! app :: FabricClient node :: Node amount :: float)
;;; ---------------------------------------------------------------------
;;; moves _node_ to the left a distance of _amount_

(define (move-player-left! app :: FabricClient node :: Node amount :: float)
  (*:setDirection app (*:normalizeLocal (*:getLeft (*:getCamera app))))
  (move-player! app node amount #f))


;;; (move-player-right! app :: FabricClient node :: Node amount :: float)
;;; ---------------------------------------------------------------------
;;; moves _node_ to the right a distance of _amount_

(define (move-player-right! app :: FabricClient node :: Node amount :: float)
  (*:setDirection app (*:normalizeLocal (*:getLeft (*:getCamera app))))
  (move-player! app node amount #t))


;;; (rotate-player-right! node :: Node amount :: float)
;;; ---------------------------------------------------------------------
;;; rotates _node_ to the right an angle of _amount_ 

(define (rotate-player-right! node :: Node amount :: float)
  (*:rotate node 0 (* -1 amount) 0))


;;; (rotate-player-left! node :: Node amount :: float)
;;; ---------------------------------------------------------------------
;;; rotates _node_ to the left an angle of _amount_ 

(define (rotate-player-left! node :: Node amount :: float)
  (*:rotate node 0 amount 0))


;;; (rotate-player-up! node :: Node amount :: float)
;;; ---------------------------------------------------------------------
;;; rotates _node_ upward an angle of _amount_ 

(define (rotate-player-up! node :: Node amount :: float)
  (*:rotate node (* -1 amount) 0 0))


;;; (rotate-player-down! node :: Node amount :: float)
;;; ---------------------------------------------------------------------
;;; rotates _node_ downward an angle of _amount_ 

(define (rotate-player-down! node :: Node amount :: float)
  (*:rotate node amount 0 0))

;;; ---------------------------------------------------------------------
;;; input-handling
;;; ---------------------------------------------------------------------

;;; (setup-inputs app::FabricClient)
;;; ---------------------------------------------------------------------
;;; establishes the event handlers that translate keypresses and
;;; mouse movements into movements of the player's node and camera

(define (setup-inputs app::FabricClient)
  ;; set up the player's controls
  (let ((key-input ::KeyInput (*:getKeyInput app))
        (input-manager (*:getInputManager app)))
    (format #t "~%setup-inputs called")
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


;;; ---------------------------------------------------------------------
;;; event-handling
;;; ---------------------------------------------------------------------

;;; (handle-analog-event app name value tpf)
;;; ---------------------------------------------------------------------
;;; handle mouse movements and other continuous events

(define (play-state-handle-analog-event state::PlayState name value tpf)
  (let* ((app::FabricClient (*:getClient state))
         (speed (*:getSpeed app))
         (node (*:getPlayerNode app))
         (right-button-down? (*:getRightButton app)))
    (format #t "~%%handle-analog-event called")
    (on-analog (name)
               ("moveForward" -> (move-player-forward! app node (* speed tpf)))
               ("maybeMoveForward" -> (when right-button-down?
                                        (move-player-forward! app node (* speed tpf))))
               ("moveBackward" -> (move-player-backward! app node (* 0.6 speed tpf)))
               ("moveRight" -> (move-player-right! app node (* speed tpf)))
               ("moveLeft" -> (move-player-left! app node (* speed tpf)))
               ("rotateRight" -> (rotate-player-right! node (* 0.25 tpf)))
               ("mouseRotateRight" -> (when right-button-down?
                                        (rotate-player-right! node value)))
               ("rotateLeft" -> (rotate-player-left! node (* 0.25 tpf)))
               ("mouseRotateLeft" -> (when right-button-down?
                                       (rotate-player-left! node value)))
               ("rotateUp" -> (rotate-player-up! node (* 0.125 tpf)))
               ("mouseRotateUp" -> (when right-button-down?
                                     (rotate-player-up! node value)))
               ("rotateDown" -> (rotate-player-down! node (* 0.125 tpf)))
               ("mouseRotateDown" -> (when right-button-down?
                                       (rotate-player-down! node value))))))

;;; (handle-action-event app name key-pressed? tpf)
;;; ---------------------------------------------------------------------
;;; handle keypresses, mouse clicks, and other discrete events

(define (play-state-handle-action-event state::PlayState name key-pressed? tpf)
  (let ((app::FabricClient (*:getClient state)))
    (on-action (name)
               ("leftButton" -> (*:setLeftButton app key-pressed?))
               ("rightButton" -> (*:setRightButton app key-pressed?)))))

;;; ---------------------------------------------------------------------
;;; PlayState functions
;;; ---------------------------------------------------------------------

(define (->texture-name name-text)
  (string-append name-text ".jpg"))

(define (prepare-to-attach-play-state state::PlayState client)
  (unless (*:getInitialized state)
          (let* ((screen::Screen (*:getScreen client))
                 (gui-node::Node (*:getGuiNode client))
                 (Align BitmapFont:Align)
                 (name-text (*:getNodeName state))
                 (texture-name (->texture-name name-text))
                 (body (make-celestial-body texture-name))
                 (sky::Spatial (make-sky-box))
                 (camera (*:getCamera client)))
            (*:setFrustumFar camera 80000)
            (*:setLocation camera (Vector3f 0.0 0.0 40000))
            (*:setCelestialBody state body)
            (*:setSky state sky)
            (setup-inputs client)
            (*:setInitialized state #t))))

(define (did-attach-play-state state::PlayState mgr::AppStateManager)
  (when (*:getInitialized state)
        (let* ((client (*:getClient state))
               (screen::Screen (*:getScreen client))
               (gui-node::Node (*:getGuiNode client)))
          (*:enqueue client
                     (runnable (lambda ()
                                 (let ((client (*:getClient state))
                                       (root::Node (*:getRootNode client)))
                                   (*:attachChild root (*:getSky state))
                                   (*:attachChild root (*:getCelestialBody state))
                                   (*:addControl gui-node screen))))))))

(define (did-detach-play-state state::PlayState mgr::AppStateManager)
  (when (*:getInitialized state)
    (let* ((client (*:getClient state))
           (screen::Screen (*:getScreen client))
           (gui-node::Node (*:getGuiNode client)))
      (*:enqueue client
                 (runnable (lambda ()
                             (let ((client (*:getClient state))
                                   (root::Node (*:getRootNode client))
                                   (sky::Spatial (*:getSky state))
                                   (body (*:getCelestialBody state)))
                               (*:detachChild root sky)
                               (*:detachChild root body)
                               (*:removeControl gui-node screen))))))))
