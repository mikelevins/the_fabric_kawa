;;; ***********************************************************************
;;;; FILE IDENTIFICATION
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
(require "view-node.scm")
(require "app-server.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(define-private-alias ActionListener com.jme3.input.controls.ActionListener)
(define-private-alias AnalogListener com.jme3.input.controls.AnalogListener)
(define-private-alias AppSettings com.jme3.system.AppSettings)
(define-private-alias AssetManager com.jme3.asset.AssetManager)
(define-private-alias BitmapFont com.jme3.font.BitmapFont)
(define-private-alias BitmapText com.jme3.font.BitmapText)
(define-private-alias BloomFilter com.jme3.post.filters.BloomFilter)
(define-private-alias CameraControl com.jme3.scene.control.CameraControl)
(define-private-alias CameraNode com.jme3.scene.CameraNode)
(define-private-alias Client com.jme3.network.Client)
(define-private-alias ChatBox tonegod.gui.controls.extras.ChatBox)
(define-private-alias ColorRGBA com.jme3.math.ColorRGBA)
(define-private-alias Container com.simsilica.lemur.Container)
(define-private-alias FilterPostProcessor com.jme3.post.FilterPostProcessor)
(define-private-alias GuiGlobals com.simsilica.lemur.GuiGlobals)
(define-private-alias KeyInput com.jme3.input.KeyInput)
(define-private-alias KeyTrigger com.jme3.input.controls.KeyTrigger)
(define-private-alias Label com.simsilica.lemur.Label)
(define-private-alias TLabel tonegod.gui.controls.text.Label)
(define-private-alias Mouse org.lwjgl.input.Mouse)
(define-private-alias MouseAxisTrigger com.jme3.input.controls.MouseAxisTrigger)
(define-private-alias MouseButtonTrigger com.jme3.input.controls.MouseButtonTrigger)
(define-private-alias MouseInput com.jme3.input.MouseInput)
(define-private-alias Network com.jme3.network.Network)
(define-private-alias Node com.jme3.scene.Node)
(define-private-alias Panel com.simsilica.lemur.Panel)
(define-private-alias PI com.jme3.math.FastMath:PI)
(define-private-alias QuadBackgroundComponent com.simsilica.lemur.component.QuadBackgroundComponent)
(define-private-alias Quaternion com.jme3.math.Quaternion)
(define-private-alias Screen tonegod.gui.core.Screen)
(define-private-alias SimpleApplication com.jme3.app.SimpleApplication)
(define-private-alias Spatial com.jme3.scene.Spatial)
(define-private-alias Styles com.simsilica.lemur.style.Styles)
(define-private-alias TbtQuadBackgroundComponent com.simsilica.lemur.component.TbtQuadBackgroundComponent)
(define-private-alias TextField com.simsilica.lemur.TextField)
(define-private-alias TTextField tonegod.gui.controls.text.TextField)
(define-private-alias Vector2f com.jme3.math.Vector2f)
(define-private-alias Vector3f com.jme3.math.Vector3f)
(define-private-alias VideoRecorderAppState com.jme3.app.state.VideoRecorderAppState)
(define-private-alias Window tonegod.gui.controls.windows.Window)

;;; ---------------------------------------------------------------------
;;; <fabric-client> - the client class
;;; ---------------------------------------------------------------------

(define-simple-class <fabric-client> (SimpleApplication AnalogListener ActionListener)

  ;; slots
  ;; -------
  (app-settings init-form: (AppSettings #t))
  (player init-form: #!null)
  (player-node :: Node init-form: #!null)
  (center-name ::java.lang.String init-form: #!null)
  (direction ::Vector3f init-form: (Vector3f))
  (network-client ::com.jme3.network.Client  init-form: #!null)
  (left-button? init-form: #f)
  (right-button? init-form: #f)
  (player-location-hud init-form: #!null)
  (chat-hud init-form: #!null)

  ;; accessors
  ;; ---------
  ((getAppSettings) app-settings)
  ((setAppSettings settings) (set! app-settings settings))
  ((getDirection) direction)
  ((setDirection dir) (set! direction dir))
  ((getCameraDirection) (*:getDirection cam))
  ((getAudioRenderer) audioRenderer)
  ((getViewport) viewPort)
  ((getInputManager) inputManager)
  ((getStateManager) stateManager)
  ((getKeyInput) keyInput)
  ((getGuiFont) guiFont)
  ((getGuiNode) guiNode)
  ((getPlayer) player)
  ((setPlayer p) (set! player p))
  ((getPlayerNode) player-node)
  ((setPlayerNode n) (set! player-node n))
  ((getCenterName) center-name)
  ((setCenterName nm) (set! center-name nm))
  ((getLeftButton) left-button?)
  ((setLeftButton down?) (set! left-button? down?))
  ((getRightButton) right-button?)
  ((setRightButton down?) (set! right-button? down?))
  ((getChatHud) chat-hud)
  ((setChatHud hud) (set! chat-hud hud))

  ;; methods
  ;; -------
  ;; network client
  ((connectToServer) (connect-client-to-server (this)))
  ((disconnectFromServer)(network-client:close))
  ((getNetworkClient) network-client)
  ((setNetworkClient client)(set! network-client client))
  ((startNetworkClient)(network-client:start))
  ((stopNetworkClient)(network-client:close))

  ;; AnalogListener and ActionListener implementation
  ((onAnalog name value tpf)(handle-analog-event (this) name value tpf))
  ((onAction name key-pressed? tpf)(handle-action-event (this) name key-pressed? tpf))

  ;; SimpleApplication implementation
  ((simpleInitApp)(init-client (this))))


;;; ---------------------------------------------------------------------
;;; <fabric-client> accessors
;;; ---------------------------------------------------------------------

(defgetter (client-app-settings <fabric-client>) getAppSettings)
(defgetter (client-audio-renderer <fabric-client>) getAudioRenderer)
(defgetter (client-camera <fabric-client>) getCamera)
(defgetter (client-camera-direction <fabric-client>) getCameraDirection)
(defgetter (client-center-name <fabric-client>) getCenterName)
(defgetter (client-chat-hud <fabric-client>) getChatHud)
(defgetter (client-direction <fabric-client>) getDirection)
(defgetter (client-fly-by-camera <fabric-client>) getFlyByCamera)
(defgetter (client-gui-font <fabric-client>) getGuiFont)
(defgetter (client-gui-node <fabric-client>) getGuiNode)
(defgetter (client-input-manager <fabric-client>) getInputManager)
(defgetter (client-key-input <fabric-client>) getKeyInput)
(defgetter (client-left-button? <fabric-client>) getLeftButton)
(defgetter (client-network-client! <fabric-client>) getNetworkClient)
(defgetter (client-player <fabric-client>) getPlayer)
(defgetter (client-player-node <fabric-client>) getPlayerNode)
(defgetter (client-right-button? <fabric-client>) getRightButton)
(defgetter (client-root-node <fabric-client>) getRootNode)
(defgetter (client-state-manager <fabric-client>) getStateManager)
(defgetter (client-viewport <fabric-client>) getViewport)

(defsetter (set-center-name! <fabric-client>) setCenterName)
(defsetter (set-client-app-settings! <fabric-client>) setAppSettings)
(defsetter (set-client-chat-hud! <fabric-client>) setChatHud)
(defsetter (set-client-direction! <fabric-client>) setDirection)
(defsetter (set-client-left-button! <fabric-client>) setLeftButton)
(defsetter (set-client-player! <fabric-client>) setPlayer)
(defsetter (set-client-player-node! <fabric-client>) setPlayerNode)
(defsetter (set-client-right-button! <fabric-client>) setRightButton)
(defsetter (set-network-client! <fabric-client>) setNetworkClient)

(define (client-camera-left app :: <fabric-client>)
  (*:getLeft (client-camera app)))

(define (client-normalize-camera! app)
  (*:normalizeLocal (client-camera-direction app)))

;;; ---------------------------------------------------------------------
;;; <fabric-client> initialization
;;; ---------------------------------------------------------------------

;;; $available-armorers
;;; ---------------------------------------------------------------------
;;; armor constructors

(define $available-armorers
  (list make-enclosing-cube
        make-enclosing-wire-cube
        make-enclosing-pyramid
        make-enclosing-sphere
        make-enclosing-wire-sphere))

;;; (make-armors n)
;;; ---------------------------------------------------------------------
;;; make N randomly-chosen armors

(define (make-armors n)
  (let ((armorers (cons make-any-plasma-generator
                        (map (lambda (a)(choose-any $available-armorers))
                             (iota n)))))
    (map (lambda (make-armor)
           (let ((armor (make-armor))
                 (rotator (any-rotator)))
             (*:addControl armor rotator)
             armor))
         armorers)))

;;; (assemble-player-character pc-node pc-geom pc-controls pc-armors)
;;; ---------------------------------------------------------------------
;;; assemble a player-character's node, geometry, controls, and armors

(define (assemble-player-character pc-node pc-geom pc-controls pc-armors)
  (*:attachChild pc-node pc-geom)
  (for-each (lambda (ctrl)
              (*:addControl pc-geom ctrl))
            pc-controls)
  (for-each (lambda (armor)
              (*:attachChild pc-node armor)
              (*:setLocalTranslation armor 0 0 0))
            pc-armors))

;;; (init-player-camera app player-node)
;;; ---------------------------------------------------------------------

(define (init-player-camera app player-node)
  (let* ((camera::com.jme3.renderer.Camera (client-camera app))
         (cam-node (CameraNode "camera" camera)))
    (*:setControlDir cam-node CameraControl:ControlDirection:SpatialToCamera)
    (*:setFrustumFar (client-camera app) 20000)
    ;; position the camera behind and above the player and look at the player
    (*:setLocalTranslation cam-node (Vector3f 0 30 -30))
    (*:lookAt cam-node (*:getLocalTranslation player-node) Vector3f:UNIT_Y)
    ;; attach the camera to the player character
    (*:attachChild player-node cam-node)))


;;; (init-player-character app ::SimpleApplication)
;;; ---------------------------------------------------------------------
;;; prepare a player character and present it in the scene

(define (init-player-character app ::SimpleApplication)
  (let* ((player-node (Node "Player"))
         (player (make-player-character (any-lit-color)))
         (player-cube (get-key player name-cube:))
         (player-rotator (any-rotator))
         ;;(armor-count (random-integer 3))
         (armor-count 0))
    (set-client-player! app player)
    (set-client-player-node! app player-node)
    ;; don't seize the mouse from the player
    (Mouse:setGrabbed #f)
    ;; disable the fly-by camera
    (*:setEnabled (client-fly-by-camera app) #f)

    ;; assemble the player character's parts
    (assemble-player-character player-node
                               player-cube
                               (list player-rotator)
                               (make-armors armor-count))
    
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
    (*:attachChild (client-root-node app) player-node)))

(define-simple-class FabricChat (ChatBox)
  ((*init* screen :: Screen id :: java.lang.String position :: Vector2f size :: Vector2f)
   (invoke-special ChatBox (this) '*init* screen id position size))
  ((onSendMsg msg::java.lang.String) #!void))

(define (init-hud app ::SimpleApplication name-string)
  (let ((screen (Screen app)))
    (*:initialize screen)
    (*:addControl (client-gui-node app) screen)
    (let* ((settings (client-app-settings app))
           (Align BitmapFont:Align)
           (VAlign BitmapFont:VAlign)
           (width (*:getWidth settings))
           (height (*:getHeight settings))
           (chatbox (FabricChat screen "chatbox"
                                (Vector2f 15 (- height 220))
                                (Vector2f 400 200)))
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

      (*:setText nodeplate (string-capitalize (client-center-name app)))
      (*:setTextAlign nodeplate Align:Left)
      (*:setFont nodeplate "Interface/Fonts/Laconic24.fnt")
      (*:setFontSize nodeplate 24)
      (*:setFontColor nodeplate ColorRGBA:Green)

      (*:setFontColor chatbox ColorRGBA:Green)
      
      (*:addElement screen nameplate)
      (*:addElement screen nodeplate)
      (*:addElement screen chatbox))))


;;; (setup-inputs app ::SimpleApplication)
;;; ---------------------------------------------------------------------
;;; set up the player's controls

(define (setup-inputs app ::SimpleApplication)
  ;; set up the player's controls
  (let ((key-input ::KeyInput (client-key-input app)))
    (*:addMapping (client-input-manager app) "moveForward" (KeyTrigger key-input:KEY_UP))
    (*:addMapping (client-input-manager app) "maybeMoveForward"
                   (MouseButtonTrigger MouseInput:BUTTON_LEFT))
    (*:addMapping (client-input-manager app) "leftButton"
                   (MouseButtonTrigger MouseInput:BUTTON_LEFT))
    (*:addMapping (client-input-manager app) "rightButton"
                   (MouseButtonTrigger MouseInput:BUTTON_RIGHT))
    (*:addMapping (client-input-manager app) "moveRight" (KeyTrigger key-input:KEY_RIGHT))
    (*:addMapping (client-input-manager app) "mouseRotateRight" (MouseAxisTrigger 0 #f))
    (*:addMapping (client-input-manager app) "moveLeft" (KeyTrigger key-input:KEY_LEFT))
    (*:addMapping (client-input-manager app) "mouseRotateLeft" (MouseAxisTrigger 0 #t))
    (*:addMapping (client-input-manager app) "mouseRotateUp" (MouseAxisTrigger 1 #f))
    (*:addMapping (client-input-manager app) "moveBackward" (KeyTrigger key-input:KEY_DOWN))
    (*:addMapping (client-input-manager app) "mouseRotateDown" (MouseAxisTrigger 1 #t))

       ;;; text inputs
    (*:addMapping (client-input-manager app) "SPACE" (KeyTrigger key-input:KEY_SPACE))
    (*:addMapping (client-input-manager app) "KEY_A" (KeyTrigger key-input:KEY_A))


    (*:addListener (client-input-manager app) app
                    ;; motion controls
                    "moveForward" "maybeMoveForward" "moveBackward" "moveRight" "moveLeft"
                    "leftButton" "rightButton" "rotateRight" "rotateLeft" "rotateUp" "rotateDown"
                    "mouseRotateRight" "mouseRotateLeft" "mouseRotateUp" "mouseRotateDown"
                    ;; chat input
                    "SPACE" "KEY_A")))


;;; (setup-lighting app ::SimpleApplication)
;;; ---------------------------------------------------------------------
;;; initialize glow and lighting effects for the scene

(define (setup-lighting app ::SimpleApplication)
  (let* ((asset-manager::AssetManager (get-asset-manager))
         (bloom (BloomFilter BloomFilter:GlowMode:Objects))
         (filter-processor::FilterPostProcessor (FilterPostProcessor asset-manager)))
    (*:setDownSamplingFactor bloom 2.0)
    (*:setBloomIntensity bloom 2.0)
    (*:addFilter filter-processor bloom)
    (*:addProcessor (client-viewport app) filter-processor)))


;;; $center-names
;;; ---------------------------------------------------------------------
;;; the names of all available celestial bodies

(define $center-names
  '("callisto"
     "dione"
     "earth"
     "enceladus"
     "europa"
     "ganymede"
     "iapetus"
     "io"
     "jupiter"
     "luna"
     "mars"
     "neptune"
     "pluto"
     "rhea"
     "saturn"
     "sedna"
     "sol"
     "titan"
     "uranus"
     "venus"))

;;; (init-client app)
;;; ---------------------------------------------------------------------
;;; set up the scene and add the player character

(define (init-client app)
  (let* ((sky (make-sky app))
         (center-body #f))

    (setup-lighting app)
    (setup-inputs app)
    (*:attachChild (client-root-node app) sky)
    (when (eq? #!null (client-center-name app))
      (set-center-name! app (choose-any $center-names)))
    (set! center-body (make-center-body app (client-center-name app)))
    (*:attachChild (client-root-node app) center-body)
    (init-player-character app)

    (let* ((player (client-player app))
           (name-strings (fabric-name-strings (get-key player name: "")))
           (player-name (call-with-output-string
                         (lambda (out)
                           (for-each (lambda (s)
                                       (format out "~a " s))
                                     name-strings)))))
      (init-hud app player-name))

    ;; uncomment to capture video to a file
    ;; (*:attach (client-state-manager app) (VideoRecorderAppState))
    #!void))


;;; ---------------------------------------------------------------------
;;; <fabric-client> event-handler functions
;;; ---------------------------------------------------------------------

(define-syntax on-analog
  (syntax-rules (->)
    ((on-analog (evt-name)
                (s -> expr) ...)
     (cond
      ((invoke evt-name 'equals s) expr) ...
      (#t #f)))))


;;; (handle-analog-event app name value tpf)
;;; ---------------------------------------------------------------------
;;; handle mouse movements and other continuous events

(define (handle-analog-event app name value tpf)
  (on-analog (name)
             ("moveForward" -> (begin (client-normalize-camera! app)
                                      (set-client-direction! app (client-camera-direction app))
                                      (*:multLocal (client-direction app) (* 300 tpf))
                                      (*:move (client-player-node app) (client-direction app))))
             ("maybeMoveForward" -> (when (client-right-button? app)
                                      (client-normalize-camera! app)
                                      (set-client-direction! app (client-camera-direction app))
                                      (*:multLocal (client-direction app) (* 300 tpf))
                                      (*:move (client-player-node app) (client-direction app))))
             ("moveBackward" -> (begin (client-normalize-camera! app)
                                       (set-client-direction! app (client-camera-direction app))
                                       (*:multLocal (client-direction app) (* -200 tpf))
                                       (*:move (client-player-node app) (client-direction app))))
             ("moveRight" -> (begin (set-client-direction! app (*:normalizeLocal (*:getLeft (client-camera app))))
                                    (*:multLocal (client-direction app) (* -150 tpf))
                                    (*:move (client-player-node app) (client-direction app))))
             ("moveLeft" -> (begin (set-client-direction! app (*:normalizeLocal (*:getLeft (client-camera app))))
                                   (*:multLocal (client-direction app) (* 150 tpf))
                                   (*:move (client-player-node app) (client-direction app))))
             ("rotateRight" -> (*:rotate (client-player-node app) 0 (* -0.25 tpf) 0))
             ("mouseRotateRight" -> (when (client-right-button? app)
                                      (*:rotate (client-player-node app) 0 (* -1 value) 0)))
             ("rotateLeft" -> (*:rotate (client-player-node app) 0 (* 0.25 tpf) 0))
             ("mouseRotateLeft" -> (when (client-right-button? app)
                                     (*:rotate (client-player-node app) 0 (* 1 value) 0)))
             ("rotateUp" -> (*:rotate (client-player-node app) (* -0.125 tpf) 0 0))
             ("mouseRotateUp" -> (when (client-right-button? app)
                                   (*:rotate (client-player-node app) (* -1 value) 0 0)))
             ("rotateDown" -> (*:rotate (client-player-node app) (* 0.125 tpf) 0 0))
             ("mouseRotateDown" -> (when (client-right-button? app)
                                     (*:rotate (client-player-node app) (* 1 value) 0 0)))))

;;; (handle-action-event app name key-pressed? tpf)
;;; ---------------------------------------------------------------------
;;; handle keypresses, mouse clicks, and other discrete events

(define-syntax on-action
  (syntax-rules (->)
    ((on-action (evt-name)
                (s -> expr) ...)
     (cond
      ((invoke evt-name 'equals s) expr) ...
      (#t #f)))))

(define (handle-action-event app name key-pressed? tpf)
  (on-action (name)
             ("leftButton" -> (set-client-left-button! app key-pressed?))
             ("rightButton" -> (set-client-right-button! app key-pressed?))))


;;; ---------------------------------------------------------------------
;;; construct the client
;;; ---------------------------------------------------------------------

;;; (make-client)
;;; ---------------------------------------------------------------------
;;; puts everything together into a runnable client

(define (make-client #!optional (center #f))
  (let* ((client :: <fabric-client> (<fabric-client>))
	 (settings :: AppSettings (client-app-settings client)))
    (when center
      (set-center-name! client center))
    (*:setResolution settings 1920 1200)
    (*:setTitle settings "The Fabric")
    (*:setSettingsDialogImage settings "Interface/icon.jpg")
    (*:setSettings client settings)
    (*:setDisplayFps client #f) ; #t to show FPS
    (*:setShowSettings client #t) ; #t to show settings dialog
    (*:setDisplayStatView client #f) ; #t to show stats
    (*:setPauseOnLostFocus client #f)
    client))
