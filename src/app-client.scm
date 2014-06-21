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
(define-private-alias ChatBox tonegod.gui.controls.extras.ChatBox)
(define-private-alias ColorRGBA com.jme3.math.ColorRGBA)
(define-private-alias FilterPostProcessor com.jme3.post.FilterPostProcessor)
(define-private-alias KeyInput com.jme3.input.KeyInput)
(define-private-alias KeyTrigger com.jme3.input.controls.KeyTrigger)
(define-private-alias Label tonegod.gui.controls.text.Label)
(define-private-alias Mouse org.lwjgl.input.Mouse)
(define-private-alias MouseAxisTrigger com.jme3.input.controls.MouseAxisTrigger)
(define-private-alias MouseButtonTrigger com.jme3.input.controls.MouseButtonTrigger)
(define-private-alias MouseInput com.jme3.input.MouseInput)
(define-private-alias Network com.jme3.network.Network)
(define-private-alias Node com.jme3.scene.Node)
(define-private-alias PI com.jme3.math.FastMath:PI)
(define-private-alias Quaternion com.jme3.math.Quaternion)
(define-private-alias Screen tonegod.gui.core.Screen)
(define-private-alias SimpleApplication com.jme3.app.SimpleApplication)
(define-private-alias Spatial com.jme3.scene.Spatial)
(define-private-alias TextField tonegod.gui.controls.text.TextField)
(define-private-alias Vector2f com.jme3.math.Vector2f)
(define-private-alias Vector3f com.jme3.math.Vector3f)
(define-private-alias VideoRecorderAppState com.jme3.app.state.VideoRecorderAppState)
(define-private-alias Window tonegod.gui.controls.windows.Window)

;;; ---------------------------------------------------------------------
;;; <fabric-client> - the main class in the client
;;; ---------------------------------------------------------------------

(define-simple-class <fabric-client> (SimpleApplication AnalogListener ActionListener)

  ;; slots
  ;; -------
  (player init-form: #!null)
  (player-node :: Node init-form: #!null)
  (center-name ::java.lang.String init-form: #!null)
  (direction ::Vector3f init-form: (Vector3f))
  (app-settings ::AppSettings init-form: (AppSettings #t))
  (network-client ::com.jme3.network.Client  init-form: #!null)
  (left-button? init-form: #f)
  (right-button? init-form: #f)
  (player-location-hud init-form: #!null)
  (chat-hud init-form: #!null)

  ;; accessors
  ;; ---------
  ((getDirection) direction)
  ((setDirection dir) (set! direction dir))
  ((getAppSettings) app-settings)
  ((setAppSettings settings) (set! app-settings settings))
  ((getCameraDirection) (@ 'getDirection cam))
  ((getAudioRenderer) audioRenderer)
  ((getViewport) viewPort)
  ((getInputManager) inputManager)
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
  ((connectToServer) (connect-client-to-server (this)))
  ((disconnectFromServer)(network-client:close))
  ((setNetworkClient client)(set! network-client client))
  ((startNetworkClient)(network-client:start))
  ;; AnalogListener and ActionListener implementation
  ((onAnalog name value tpf)(handle-analog-event (this) name value tpf))
  ((onAction name key-pressed? tpf)(handle-action-event (this) name key-pressed? tpf))
  ;; SimpleApplication implementation
  ((simpleInitApp)(init-client (this))))

;;; ---------------------------------------------------------------------
;;; <fabric-client> accessors
;;; ---------------------------------------------------------------------

(define (camera-direction app  :: <fabric-client>)(@ 'getCameraDirection app))
(define (camera-left app  :: <fabric-client>)(@ 'getLeft (@ 'getCamera app)))
(define (center-name app ::SimpleApplication)(@ 'getCenterName app))

(define (client-app-settings app ::SimpleApplication)(@ 'getAppSettings app))
(define (client-audio-renderer app ::SimpleApplication)(@ 'getAudioRenderer app))
(define (client-camera app ::SimpleApplication)(@ 'getCamera app))
(define (client-chat-hud app ::SimpleApplication)(@ 'getChatHud app))
(define (client-direction app  :: <fabric-client>)(@ 'getDirection app))
(define (client-gui-font app ::SimpleApplication)(@ 'getGuiFont app))
(define (client-gui-node app ::SimpleApplication)(@ 'getGuiNode app))
(define (client-input-manager app ::SimpleApplication)(@ 'getInputManager app))
(define (client-key-input app ::SimpleApplication)(@ 'getKeyInput app))
(define (client-player app ::SimpleApplication)(@ 'getPlayer app))
(define (client-player-node app ::SimpleApplication)(@ 'getPlayerNode app))
(define (client-viewport app ::SimpleApplication)(@ 'getViewport app))

(define (fly-by-camera app ::SimpleApplication)(@ 'getFlyByCamera app))
(define (left-button? app)(@ 'getLeftButton app))
(define (normalize-camera! app)(@ 'normalizeLocal (camera-direction app)))
(define (right-button? app)(@ 'getRightButton app))
(define (root-node app ::SimpleApplication)(@ 'getRootNode app))

(define (set-center-name! app ::SimpleApplication name :: java.lang.String)(@ 'setCenterName app name))
(define (set-client-app-settings! app ::SimpleApplication settings)(@ 'setAppSettings app settings))
(define (set-client-chat-hud! app ::SimpleApplication hud)(@ 'setChatHud app hud))
(define (set-client-direction! app  :: <fabric-client> dir)(@ 'setDirection app dir))
(define (set-client-player! app ::SimpleApplication player)(@ 'setPlayer app player))
(define (set-client-player-node! app ::SimpleApplication node)(@ 'setPlayerNode app node))
(define (set-left-button! app key-pressed?)(@ 'setLeftButton app key-pressed?))
(define (set-network-client! app  :: <fabric-client> client)(invoke app 'setNetworkClient client))
(define (set-right-button! app key-pressed?)(@ 'setRightButton app key-pressed?))
(define (start-network-client! app  :: <fabric-client>)(invoke app 'startNetworkClient))


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
        make-enclosing-wire-sphere
        make-any-plasma-generator))

;;; (make-armors n)
;;; ---------------------------------------------------------------------
;;; make N randomly-chosen armors

(define (make-armors n)
  (let ((armorers (map (lambda (a)(choose-any $available-armorers))
                       (iota n))))
    (map (lambda (make-armor)
           (let ((armor (make-armor))
                 (rotator (any-rotator)))
             (@ 'addControl armor rotator)
             armor))
         armorers)))

;;; (assemble-player-character pc-node pc-geom pc-controls pc-armors)
;;; ---------------------------------------------------------------------
;;; assemble a player-character's node, geometry, controls, and armors

(define (assemble-player-character pc-node pc-geom pc-controls pc-armors)
  (@ 'attachChild pc-node pc-geom)
  (for-each (lambda (ctrl)
              (@ 'addControl pc-geom ctrl))
            pc-controls)
  (for-each (lambda (armor)
              (@ 'attachChild pc-node armor)
              (@ 'setLocalTranslation armor 0 0 0))
            pc-armors))


;;; (init-player-camera app player-node)
;;; ---------------------------------------------------------------------

(define (init-player-camera app player-node)
  (let* ((camera::com.jme3.renderer.Camera (client-camera app))
         (cam-node (CameraNode "camera" camera)))
    (invoke cam-node 'setControlDir CameraControl:ControlDirection:SpatialToCamera)
    (@ 'setFrustumFar (client-camera app) 20000)
    ;; position the camera behind and above the player and look at the player
    (@ 'setLocalTranslation cam-node (Vector3f 0 30 -30))
    (@ 'lookAt cam-node (@ 'getLocalTranslation player-node) Vector3f:UNIT_Y)
    ;; attach the camera to the player character
    (@ 'attachChild player-node cam-node)))


;;; (init-player-character app ::SimpleApplication)
;;; ---------------------------------------------------------------------
;;; prepare a player character and present it in the scene

(define (init-player-character app ::SimpleApplication)
  (let* ((player-node (Node "Player"))
         (player (make-player-character (any-lit-color)))
         (player-cube (get-key player name-cube:))
         (player-rotator (any-rotator))
         (armor-count (random-integer 3)))
    (set-client-player! app player)
    (set-client-player-node! app player-node)
    ;; don't seize the mouse from the player
    (Mouse:setGrabbed #f)
    ;; disable the fly-by camera
    (@ 'setEnabled (fly-by-camera app) #f)

    ;; assemble the player character's parts
    (assemble-player-character player-node
                               player-cube
                               (list player-rotator)
                               (make-armors armor-count))
    
    ;; set up the player character's camera
    (init-player-camera app player-node)

    ;; move the character to its starting location and point it at the center
    (@ 'setLocalTranslation player-node 0.0 8000.0 0.0)
    (let ((rotation (Quaternion))
          (pitch-axis (Vector3f 1 0 0)))
      ;; PI/4 radians points us right at the center
      (@ 'fromAngleAxis rotation (/ PI 4) pitch-axis)
      (@ 'setLocalRotation player-node rotation))
    
    ;; add the player to the scene
    (@ 'attachChild (root-node app) player-node)))


;;; (init-hud app ::SimpleApplication)
;;; ---------------------------------------------------------------------
;;; initialize the heads-up display

(define (init-hud app ::SimpleApplication name-string)
  (let ((screen (Screen app)))
    (@ 'initialize screen)
    (@ 'addControl (client-gui-node app) screen)
    (let* ((Align BitmapFont:Align)
           (VAlign BitmapFont:VAlign)
           (width (@ 'getWidth (client-app-settings app)))
           (height (@ 'getHeight (client-app-settings app)))
           (chatbox (Window screen "chatbox"
                            (Vector2f 15 (- height 220))
                            (Vector2f 400 200)))
           (nameplate (Label screen "nameplate"
                             (Vector2f (* 9 (/ width 24.0)) (- height 44))
                             (Vector2f 600 40))))
      (@ 'setText nameplate name-string)
      (@ 'setTextAlign nameplate Align:Center)
      ;;(@ 'setFont nameplate "Interface/Fonts/SourceCodePro.fnt")
      (@ 'setFont nameplate "Interface/Fonts/Orbitron36.fnt")
      (@ 'setFontSize nameplate 32)
      (@ 'setFontColor nameplate ColorRGBA:Green)

      (@ 'setFontColor chatbox ColorRGBA:Green)
      (@ 'setWindowTitle chatbox "Chat")

      
      (@ 'addElement screen nameplate)
      (@ 'addElement screen chatbox))))



;;; (setup-inputs app ::SimpleApplication)
;;; ---------------------------------------------------------------------
;;; set up the player's controls

(define (setup-inputs app ::SimpleApplication)
  ;; set up the player's controls
  (let ((key-input ::KeyInput (client-key-input app)))
    (@ 'addMapping (client-input-manager app) "moveForward" (KeyTrigger key-input:KEY_UP))
    (@ 'addMapping (client-input-manager app) "maybeMoveForward"
                   (MouseButtonTrigger MouseInput:BUTTON_LEFT))
    (@ 'addMapping (client-input-manager app) "leftButton"
                   (MouseButtonTrigger MouseInput:BUTTON_LEFT))
    (@ 'addMapping (client-input-manager app) "rightButton"
                   (MouseButtonTrigger MouseInput:BUTTON_RIGHT))
    (@ 'addMapping (client-input-manager app) "moveRight" (KeyTrigger key-input:KEY_RIGHT))
    (@ 'addMapping (client-input-manager app) "mouseRotateRight" (MouseAxisTrigger 0 #f))
    (@ 'addMapping (client-input-manager app) "moveLeft" (KeyTrigger key-input:KEY_LEFT))
    (@ 'addMapping (client-input-manager app) "mouseRotateLeft" (MouseAxisTrigger 0 #t))
    (@ 'addMapping (client-input-manager app) "mouseRotateUp" (MouseAxisTrigger 1 #f))
    (@ 'addMapping (client-input-manager app) "moveBackward" (KeyTrigger key-input:KEY_DOWN))
    (@ 'addMapping (client-input-manager app) "mouseRotateDown" (MouseAxisTrigger 1 #t))

       ;;; text inputs
    (@ 'addMapping (client-input-manager app) "SPACE" (KeyTrigger key-input:KEY_SPACE))
    (@ 'addMapping (client-input-manager app) "KEY_A" (KeyTrigger key-input:KEY_A))


    (@ 'addListener (client-input-manager app) app
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
    (@ 'setDownSamplingFactor bloom 2.0)
    (@ 'setBloomIntensity bloom 2.0)
    (@ 'addFilter filter-processor bloom)
    (@ 'addProcessor (client-viewport app) filter-processor)))


;;; $center-names
;;; ---------------------------------------------------------------------
;;; the names of all available celestial bodies

(define $center-names
  '("callisto"
     "earth"
     "enceladus"
     "europa"
     "ganymede"
     "iapetus"
     "io"
     "jupiter"
     "mars"
     "neptune"
     "pluto"
     "rhea"
    "the_sun"))

;;; (init-client app)
;;; ---------------------------------------------------------------------
;;; set up the scene and add the player character

(define (init-client app)
  (let* ((sky (make-sky app))
        (center-name (choose-any $center-names))
        (center-body (make-center-body app center-name)))

    (setup-lighting app)
    (setup-inputs app)
    (@ 'attachChild (root-node app) sky)
    (set-center-name! app center-name)
    (@ 'attachChild (root-node app) center-body)
    (init-player-character app)

    (let* ((player (client-player app))
           (player-name (format #f "~a" (fabric-name-strings (get-key player name: "")))))
      (init-hud app player-name))

    ;; uncomment to capture video to a file
    ;;(@ 'attach stateManager (VideoRecorderAppState))
    #!void))


;;; (init-network-client)
;;; ---------------------------------------------------------------------
;;; connect to the game server

(define (init-network-client)
  (com.jme3.network.Network:connectToServer (server-address)(server-port)))

;;; (connect-client-to-server app)
;;; ---------------------------------------------------------------------

(define (connect-client-to-server app)
  (begin (set-network-client! app (init-network-client))
         (start-network-client! app)))


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
             ("moveForward" -> (begin (normalize-camera! app)
                                      (set-client-direction! app (camera-direction app))
                                      (@ 'multLocal (client-direction app) (* 300 tpf))
                                      (@ 'move (client-player-node app) (client-direction app))))
             ("maybeMoveForward" -> (when (right-button? app)
                                      (normalize-camera! app)
                                      (set-client-direction! app (camera-direction app))
                                      (@ 'multLocal (client-direction app) (* 300 tpf))
                                      (@ 'move (client-player-node app) (client-direction app))))
             ("moveBackward" -> (begin (normalize-camera! app)
                                       (set-client-direction! app (camera-direction app))
                                       (@ 'multLocal (client-direction app) (* -200 tpf))
                                       (@ 'move (client-player-node app) (client-direction app))))
             ("moveRight" -> (begin (set-client-direction! app (@ 'normalizeLocal (@ 'getLeft (client-camera app))))
                                    (@ 'multLocal (client-direction app) (* -150 tpf))
                                    (@ 'move (client-player-node app) (client-direction app))))
             ("moveLeft" -> (begin (set-client-direction! app (@ 'normalizeLocal (@ 'getLeft (client-camera app))))
                                   (@ 'multLocal (client-direction app) (* 150 tpf))
                                   (@ 'move (client-player-node app) (client-direction app))))
             ("rotateRight" -> (@ 'rotate (client-player-node app) 0 (* -0.25 tpf) 0))
             ("mouseRotateRight" -> (when (right-button? app)
                                      (@ 'rotate (client-player-node app) 0 (* -1 value) 0)))
             ("rotateLeft" -> (@ 'rotate (client-player-node app) 0 (* 0.25 tpf) 0))
             ("mouseRotateLeft" -> (when (right-button? app)
                                     (@ 'rotate (client-player-node app) 0 (* 1 value) 0)))
             ("rotateUp" -> (@ 'rotate (client-player-node app) (* -0.125 tpf) 0 0))
             ("mouseRotateUp" -> (when (right-button? app)
                                   (@ 'rotate (client-player-node app) (* -1 value) 0 0)))
             ("rotateDown" -> (@ 'rotate (client-player-node app) (* 0.125 tpf) 0 0))
             ("mouseRotateDown" -> (when (right-button? app)
                                     (@ 'rotate (client-player-node app) (* 1 value) 0 0)))))

;;; (handle-action-event app name key-pressed? tpf)
;;; ---------------------------------------------------------------------
;;; handle keypresses, mouse clicks, and other discrete events


;; (define-syntax on-action
;;   (syntax-rules (->)
;;     ((on-analog (evt-name)
;;                 (s -> expr) ...)
;;      (cond
;;       ((invoke evt-name 'equals s) expr) ...
;;       (#t #f)))))


(define (handle-action-event app name key-pressed? tpf)
  (cond
   ((@ 'equals name "leftButton")(set-left-button! app key-pressed?))
   ((@ 'equals name "rightButton")(set-right-button! app key-pressed?))
   ;; add typed text characters to chat
   (#t 
    (if key-pressed?
        #!void
        (cond
         ((@ 'equals name "SPACE")(let* ((old-text (@ 'getText (client-chat-hud app)))
                                         (new-text (string-append old-text " ")))
                                    (@ 'setText (client-chat-hud app) new-text)))
         ((@ 'equals name "KEY_A")(let* ((old-text (@ 'getText (client-chat-hud app)))
                                         (new-text (string-append old-text "A")))
                                    (@ 'setText (client-chat-hud app) new-text)))
         (#t #f))))))


;;; ---------------------------------------------------------------------
;;; construct the client
;;; ---------------------------------------------------------------------

;;; (make-client)
;;; ---------------------------------------------------------------------
;;; puts everything together into a runnable client

(define (make-client)
  (let* ((client :: <fabric-client> (<fabric-client>))
	 (settings::AppSettings (invoke client 'getAppSettings)))
    (@ 'setResolution settings 1920 1200)
    (@ 'setTitle settings "The Fabric")
    (@ 'setSettingsDialogImage settings "Interface/icon.jpg")
    (@ 'setSettings client settings)
    (@ 'setDisplayFps client #f) ; #t to show FPS
    (@ 'setShowSettings client #t) ; #t to show settings dialog
    (@ 'setDisplayStatView client #f) ; #t to show stats
    (@ 'setPauseOnLostFocus client #f)
    client))


