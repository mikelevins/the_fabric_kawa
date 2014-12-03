;;; ***********************************************************************
;;;;
;;;; Name:          client.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       client main program
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export make-client)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require 'list-lib)
(require "util-java.scm")
(require "syntax-classes.scm")
(require "util-general.scm")
(require "util-random.scm")
(require "assets-general.scm")
(require "model-frame.scm")
(require "model-namegen.scm")
(require "util-lists.scm")
(require "view-shapes.scm")
(require "view-controls.scm")
(require "view-colors.scm")
(require "view-plasma.scm")
(require "view-player.scm")
(require "init-config.scm")
(require "view-node.scm")
(require "syntax-events.scm")
(require "ui-chatbox.scm")
(require "app-common.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as AppSettings com.jme3.system.AppSettings)
(import-as AssetManager com.jme3.asset.AssetManager)
(import-as BitmapFont com.jme3.font.BitmapFont)
(import-as BloomFilter com.jme3.post.filters.BloomFilter)
(import-as CameraControl com.jme3.scene.control.CameraControl)
(import-as CameraNode com.jme3.scene.CameraNode)
(import-as Client com.jme3.network.Client)
(import-as ColorRGBA com.jme3.math.ColorRGBA)
(import-as ConnectException java.net.ConnectException)
(import-as EffectEvent tonegod.gui.effects.Effect:EffectEvent)
(import-as FilterPostProcessor com.jme3.post.FilterPostProcessor)
(import-as KeyInput com.jme3.input.KeyInput)
(import-as KeyTrigger com.jme3.input.controls.KeyTrigger)
(import-as TLabel tonegod.gui.controls.text.Label)
(import-as Mouse org.lwjgl.input.Mouse)
(import-as MouseAxisTrigger com.jme3.input.controls.MouseAxisTrigger)
(import-as MouseButtonTrigger com.jme3.input.controls.MouseButtonTrigger)
(import-as MouseInput com.jme3.input.MouseInput)
(import-as Node com.jme3.scene.Node)
(import-as PI com.jme3.math.FastMath:PI)
(import-as Quaternion com.jme3.math.Quaternion)
(import-as Screen tonegod.gui.core.Screen)
(import-as Serializer com.jme3.network.serializing.Serializer)
(import-as String java.lang.String)
(import-as Vector2f com.jme3.math.Vector2f)
(import-as Vector3f com.jme3.math.Vector3f)

;;; ---------------------------------------------------------------------
;;; FabricClient - the client class
;;; ---------------------------------------------------------------------

(defclass FabricClient (FabricApp)
  (slots:
   (player init-form: #!null getter: getPlayer setter: setPlayer)
   (player-node type: Node init-form: #!null getter: getPlayerNode setter: setPlayerNode))

  (methods:
   ((onAnalog name value tpf)(handle-analog-event (this) name value tpf))
   ((onAction name key-pressed? tpf)(handle-action-event (this) name key-pressed? tpf))
   ((simpleInitApp)(init-client (this)))))


;;; ---------------------------------------------------------------------
;;; player setup
;;; ---------------------------------------------------------------------

;;; assemble a player character
;;; ---------------------------------------------------------------------

(define (assemble-player-character pc-node pc-geom pc-controls pc-armors)
  (*:attachChild pc-node pc-geom)
  (for-each (lambda (ctrl)
              (*:addControl pc-geom ctrl))
            pc-controls)
  (for-each (lambda (armor)
              (*:attachChild pc-node armor)
              (*:setLocalTranslation armor 0 0 0))
            pc-armors))

;;; set up the player camera
;;; ---------------------------------------------------------------------

(define (init-player-camera app player-node)
  (let* ((camera::com.jme3.renderer.Camera (*:getCamera app))
         (cam-node (CameraNode "camera" camera)))
    (*:setControlDir cam-node CameraControl:ControlDirection:SpatialToCamera)
    (*:setFrustumFar (*:getCamera app) 20000)
    ;; position the camera behind and above the player and look at the player
    (*:setLocalTranslation cam-node (Vector3f 0 30 -30))
    (*:lookAt cam-node (*:getLocalTranslation player-node) Vector3f:UNIT_Y)
    ;; attach the camera to the player character
    (*:attachChild player-node cam-node)))

;;; prepare a player character and present it in the scene
;;; ---------------------------------------------------------------------

(define (init-player-character app ::FabricApp)
  (let* ((player-node (Node "Player"))
         (player (make-player-character (any-lit-color)))
         (player-cube (get-key player name-cube:))
         (player-rotator (any-rotator))
         ;;(armor-count (random-integer 3))
         (armor-count 2))
    (*:setPlayer app player)
    (*:setPlayerNode app player-node)
    ;; don't seize the mouse from the player
    (Mouse:setGrabbed #f)
    ;; disable the fly-by camera
    (*:setEnabled (*:getFlyByCamera app) #f)

    ;; assemble the player character's parts
    (assemble-player-character player-node
                               player-cube
                               (list player-rotator)
                               (make-armors armor-count)
                               ;; with no armors
                               ;;'()
                               )
    
    ;; set up the player character's camera
    (init-player-camera app player-node)

    ;; move the character to its starting location and point it at the center
    (*:setLocalTranslation player-node 0.0 8000.0 0.0)
    (let ((rotation (Quaternion))
          (pitch-axis (Vector3f 1 0 0)))
      ;; PI/4 radians points us right at the center
      (*:fromAngleAxis rotation (/ PI 4) pitch-axis)
      (*:setLocalRotation player-node rotation))
    
    ;; add the player to the scene
    (*:attachChild (*:getRootNode app) player-node)))

;;; ---------------------------------------------------------------------
;;; assemble the HUD
;;; ---------------------------------------------------------------------

(define (init-hud app ::FabricApp name-string)
  (let ((screen (Screen app))
        (key-input ::KeyInput (*:getKeyInput app)))
    (*:initialize screen)
    (*:addControl (*:getGuiNode app) screen)
    (let* ((settings (*:getAppSettings app))
           (Align BitmapFont:Align)
           (VAlign BitmapFont:VAlign)
           (width (*:getWidth settings))
           (height (*:getHeight settings))
           (chatbox (FabricChat screen "chatbox"
                                (Vector2f 15 (- height 220))
                                (Vector2f 400 200)))
           (chatfield (*:getChildElementById chatbox "chatbox:ChatInput"))
           (chatlog (*:getChildElementById chatbox "chatbox:ChatArea"))
           (nameplate (TLabel screen "nameplate"
                              (Vector2f 8 8)
                              (Vector2f 900 40)))
           (nodeplate (TLabel screen "nodeplate"
                              (Vector2f 8 48)
                              (Vector2f 900 40))))

      (*:setText nameplate name-string)
      (*:setTextAlign nameplate Align:Left)
      (*:setFont nameplate "Interface/Fonts/Laconic30.fnt")
      (*:setFontSize nameplate 30)
      (*:setFontColor nameplate ColorRGBA:Green)

      (*:setText nodeplate (string-capitalize (*:getCenterName app)))
      (*:setTextAlign nodeplate Align:Left)
      (*:setFont nodeplate "Interface/Fonts/Laconic24.fnt")
      (*:setFontSize nodeplate 24)
      (*:setFontColor nodeplate ColorRGBA:Green)

      (*:setFontColor chatlog ColorRGBA:Green)
      (*:removeEffect chatfield EffectEvent:TabFocus)
      (*:setFontSize chatfield 24)
      (*:setSendKey chatbox key-input:KEY_RETURN)
      (*:setChatName chatbox (player-namestring (*:getPlayer app)))
      
      (*:setChatHud app chatbox)
      (*:addElement screen nameplate)
      (*:addElement screen nodeplate)
      (*:addElement screen chatbox))))

;;; ---------------------------------------------------------------------
;;; set up player inputs
;;; ---------------------------------------------------------------------

(define (setup-inputs app ::FabricApp)
  ;; set up the player's controls
  (let ((key-input ::KeyInput (*:getKeyInput app))
        (input-manager (*:getInputManager app)))
    (*:addMapping input-manager "moveForward" (KeyTrigger key-input:KEY_UP))
    (*:addMapping input-manager "moveForward" (KeyTrigger key-input:KEY_W))
    (*:addMapping input-manager "maybeMoveForward" (MouseButtonTrigger MouseInput:BUTTON_LEFT))
    (*:addMapping input-manager "leftButton" (MouseButtonTrigger MouseInput:BUTTON_LEFT))
    (*:addMapping input-manager "rightButton" (MouseButtonTrigger MouseInput:BUTTON_RIGHT))
    (*:addMapping input-manager "moveRight" (KeyTrigger key-input:KEY_RIGHT))
    (*:addMapping input-manager "moveRight" (KeyTrigger key-input:KEY_D))
    (*:addMapping input-manager "mouseRotateRight" (MouseAxisTrigger 0 #f))
    (*:addMapping input-manager "moveLeft" (KeyTrigger key-input:KEY_LEFT))
    (*:addMapping input-manager "moveLeft" (KeyTrigger key-input:KEY_A))
    (*:addMapping input-manager "mouseRotateLeft" (MouseAxisTrigger 0 #t))
    (*:addMapping input-manager "mouseRotateUp" (MouseAxisTrigger 1 #f))
    (*:addMapping input-manager "moveBackward" (KeyTrigger key-input:KEY_DOWN))
    (*:addMapping input-manager "moveBackward" (KeyTrigger key-input:KEY_S))
    (*:addMapping input-manager "mouseRotateDown" (MouseAxisTrigger 1 #t))

    ;; text inputs
    (*:addMapping input-manager "SPACE" (KeyTrigger key-input:KEY_SPACE))
    (*:addMapping input-manager "KEY_A" (KeyTrigger key-input:KEY_A))

    ;; set up the event listener
    (*:addListener input-manager app
                   ;; motion controls
                   "moveForward" "maybeMoveForward" "moveBackward" "moveRight" "moveLeft"
                   "leftButton" "rightButton" "rotateRight" "rotateLeft" "rotateUp" "rotateDown"
                   "mouseRotateRight" "mouseRotateLeft" "mouseRotateUp" "mouseRotateDown"
                   ;; chat input
                   "SPACE" "KEY_A")))

;;; ---------------------------------------------------------------------
;;; set up the scene
;;; ---------------------------------------------------------------------

;;; lighting
;;; ---------------------------------------------------------------------

(define (setup-lighting app ::FabricApp)
  (let* ((asset-manager::AssetManager (get-asset-manager))
         (bloom (BloomFilter BloomFilter:GlowMode:Objects))
         (filter-processor::FilterPostProcessor (FilterPostProcessor asset-manager)))
    (*:setDownSamplingFactor bloom 2.0)
    (*:setBloomIntensity bloom 2.0)
    (*:addFilter filter-processor bloom)
    (*:addProcessor (*:getViewport app) filter-processor)))


;;; initializing the app
;;; ---------------------------------------------------------------------

(define (init-client app)
  (let* ((sky (make-sky app))
         ;;(center-body #f)
         )

    ;; set up the scene
    (setup-lighting app)
    (setup-inputs app)
    (*:attachChild (*:getRootNode app) sky)
    (when (eq? #!null (*:getCenterName app))
      (*:setCenterName app (choose-any (node-names))))
    ;;(set! center-body (make-center-body app (*:getCenterName app)))
    ;;(*:attachChild (*:getRootNode app) center-body)
    (init-player-character app)

    ;; set up connectivity
    (ensure-valid-network-client app)
    ;; set up the UI
    (let ((player (*:getPlayer app)))
      (init-hud app (player-namestring player)))

    ;; return void to make Java happy
    #!void))

;;; ---------------------------------------------------------------------
;;; set up event-handling
;;; ---------------------------------------------------------------------

;;; (handle-analog-event app name value tpf)
;;; ---------------------------------------------------------------------
;;; handle mouse movements and other continuous events

(define (handle-analog-event app name value tpf)
  (let ((speed (*:getSpeed app))
        (node (*:getPlayerNode app))
        (right-button-down? (*:getRightButton app)))
    (on-analog (name)
               ("moveForward" -> (move-node-forward! app node (* speed tpf)))
               ("maybeMoveForward" -> (when right-button-down?
                                        (move-node-forward! app node (* speed tpf))))
               ("moveBackward" -> (move-node-backward! app node (* 0.6 speed tpf)))
               ("moveRight" -> (move-node-right! app node (* speed tpf)))
               ("moveLeft" -> (move-node-left! app node (* speed tpf)))
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

;;; (handle-action-event app name key-pressed? tpf)
;;; ---------------------------------------------------------------------
;;; handle keypresses, mouse clicks, and other discrete events

(define (handle-action-event app name key-pressed? tpf)
  (on-action (name)
             ("leftButton" -> (*:setLeftButton app key-pressed?))
             ("rightButton" -> (*:setRightButton app key-pressed?))))

;;; ---------------------------------------------------------------------
;;; construct the client app
;;; ---------------------------------------------------------------------

(define (make-client #!optional (center #f))
  (let* ((client :: FabricClient (FabricClient))
	 (settings :: AppSettings (*:getAppSettings client)))
    (when center (*:setCenterName client center))
    (Serializer:registerClass ChatMessage)
    (*:setResolution settings 1920 1200)
    (*:setTitle settings "The Fabric")
    (*:setSettingsDialogImage settings "Interface/icon.jpg")
    (*:setSettings client settings)
    (*:setSpeed client 300.0)
    (*:setDisplayFps client #f) ; #t to show FPS
    (*:setShowSettings client #t) ; #t to show settings dialog
    (*:setDisplayStatView client #f) ; #t to show stats
    (*:setPauseOnLostFocus client #t)
    client))
