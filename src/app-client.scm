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

(module-export make-client client-network-client send-chat-message)

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
(require "net-messaging.scm")
(require "util-lists.scm")
(require "view-shapes.scm")
(require "view-controls.scm")
(require "view-colors.scm")
(require "view-plasma.scm")
(require "view-player.scm")
(require "init-config.scm")
(require "view-node.scm")
(require "syntax-events.scm")
(require "app-common.scm")

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
(define-private-alias ConnectException java.net.ConnectException)
(define-private-alias Container com.simsilica.lemur.Container)
(define-private-alias EffectEvent tonegod.gui.effects.Effect:EffectEvent)
(define-private-alias FilterPostProcessor com.jme3.post.FilterPostProcessor)
(define-private-alias GuiGlobals com.simsilica.lemur.GuiGlobals)
(define-private-alias KeyInput com.jme3.input.KeyInput)
(define-private-alias KeyTrigger com.jme3.input.controls.KeyTrigger)
(define-private-alias Label com.simsilica.lemur.Label)
(define-private-alias TLabel tonegod.gui.controls.text.Label)
(define-private-alias MessageListener com.jme3.network.MessageListener)
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
(define-private-alias Serializable com.jme3.network.serializing.Serializable)
(define-private-alias Serializer com.jme3.network.serializing.Serializer)
(define-private-alias SimpleApplication com.jme3.app.SimpleApplication)
(define-private-alias Spatial com.jme3.scene.Spatial)
(define-private-alias String java.lang.String)
(define-private-alias Styles com.simsilica.lemur.style.Styles)
(define-private-alias TbtQuadBackgroundComponent com.simsilica.lemur.component.TbtQuadBackgroundComponent)
(define-private-alias TextField com.simsilica.lemur.TextField)
(define-private-alias TTextField tonegod.gui.controls.text.TextField)
(define-private-alias Vector2f com.jme3.math.Vector2f)
(define-private-alias Vector3f com.jme3.math.Vector3f)
(define-private-alias VideoRecorderAppState com.jme3.app.state.VideoRecorderAppState)
(define-private-alias Window tonegod.gui.controls.windows.Window)

;;; ---------------------------------------------------------------------
;;; FabricClient - the client class
;;; ---------------------------------------------------------------------

(define-simple-class FabricClient (FabricApp)

  ;; slots
  ;; -------
  (player init-form: #!null)
  (player-node :: Node init-form: #!null)
  (center-name ::String init-form: #!null)
  (direction ::Vector3f init-form: (Vector3f))
  (network-client ::com.jme3.network.Client  init-form: #!null)
  (left-button? init-form: #f)
  (right-button? init-form: #f)
  (player-location-hud init-form: #!null)
  (chat-hud init-form: #!null)

  ;; accessors
  ;; ---------
  ((getDirection) direction)
  ((setDirection dir) (set! direction dir))
  ((getNetworkClient) network-client)
  ((setNetworkClient client) (set! network-client client))
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

  ;; AnalogListener and ActionListener implementation
  ((onAnalog name value tpf)(handle-analog-event (this) name value tpf))
  ((onAction name key-pressed? tpf)(handle-action-event (this) name key-pressed? tpf))
  ;; SimpleApplication implementation
  ((simpleInitApp)(init-client (this))))

;;; ---------------------------------------------------------------------
;;; FabricClient accessors
;;; ---------------------------------------------------------------------

(defgetter (client-audio-renderer FabricClient) getAudioRenderer)
(defgetter (client-camera FabricClient) getCamera)
(defgetter (client-camera-direction FabricClient) getCameraDirection)
(defgetter (client-center-name FabricClient) getCenterName)
(defgetter (client-chat-hud FabricClient) getChatHud)
(defgetter (client-direction FabricClient) getDirection)
(defgetter (client-network-client FabricClient) getNetworkClient)
(defgetter (client-fly-by-camera FabricClient) getFlyByCamera)
(defgetter (client-gui-font FabricClient) getGuiFont)
(defgetter (client-gui-node FabricClient) getGuiNode)
(defgetter (client-input-manager FabricClient) getInputManager)
(defgetter (client-key-input FabricClient) getKeyInput)
(defgetter (client-left-button? FabricClient) getLeftButton)
(defgetter (client-player FabricClient) getPlayer)
(defgetter (client-player-node FabricClient) getPlayerNode)
(defgetter (client-right-button? FabricClient) getRightButton)
(defgetter (client-root-node FabricClient) getRootNode)
(defgetter (client-state-manager FabricClient) getStateManager)
(defgetter (client-viewport FabricClient) getViewport)

(defsetter (set-center-name! FabricClient) setCenterName)
(defsetter (set-client-chat-hud! FabricClient) setChatHud)
(defsetter (set-client-direction! FabricClient) setDirection)
(defsetter (set-client-network-client! FabricClient) setNetworkClient)
(defsetter (set-client-left-button! FabricClient) setLeftButton)
(defsetter (set-client-player! FabricClient) setPlayer)
(defsetter (set-client-player-node! FabricClient) setPlayerNode)
(defsetter (set-client-right-button! FabricClient) setRightButton)

(define (client-camera-left app :: FabricClient)
  (*:getLeft (client-camera app)))

(define (client-normalize-camera! app)
  (*:normalizeLocal (client-camera-direction app)))

;;; ---------------------------------------------------------------------
;;; set up the player character
;;; ---------------------------------------------------------------------

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

;;; ---------------------------------------------------------------------
;;; ClientChatHandler - aux class for handling incoming chat messages
;;; ---------------------------------------------------------------------

(define-simple-class ClientChatHandler (MessageListener)
  (application init-form: #!null)
  ((*init* app)(set! application app))
  ((messageReceived source msg) (if (instance? msg ChatMessage)
                                    (let* ((chatbox (client-chat-hud application))
                                           (msg-name (message-name msg))
                                           (msg-contents (message-contents msg))
                                           (received-text (format #f "[~A] ~A"
                                                                  msg-name msg-contents))
                                           (updater (runnable (lambda ()
                                                                (*:receiveMsg chatbox received-text)))))
                                      (*:enqueue application updater))
                                    (format #t "Unrecognized message: ~s" msg))))

;;; ---------------------------------------------------------------------
;;; set up the heads-up display and chatbox
;;; ---------------------------------------------------------------------

(define (client-report-failed-chat-message app chat-message chat-box)
  (let* ((msg-name (message-name chat-message))
         (msg-contents (message-contents chat-message))
         (failed-text (format #f "Connection failed; unable to send message: [~A] ~A"
                              msg-name msg-contents)))
    (*:receiveMsg chat-box failed-text)))

(define (client-connect-to-server app)
  (try-catch
   (let ((new-connection (Network:connectToServer (server-name)(server-version)(server-host)
                                                  (server-port)(server-port))))
     (set-client-network-client! app new-connection)
     (*:addMessageListener (client-network-client app) (ClientChatHandler app))
     (*:start (client-network-client app)))
   (ex java.net.ConnectException (begin (set-client-network-client! app #!null)
                                        (format #t "~%Failed to connect to Fabric server.")
                                        (format #t "~%~A" (*:toString ex))))))

(define (ensure-valid-network-client app)
  (let ((net-client #f)
        (found-client (client-network-client app)))
    (when (jnull? found-client)
      (client-connect-to-server app))
    (set! net-client (client-network-client app))
    (if (jnull? net-client)
        #f
        (if (*:isConnected net-client)
            net-client
            #f))))

(define (send-chat-message app chat-message)
  (let ((net-client (ensure-valid-network-client app)))
    (if net-client
        (*:send net-client chat-message)
        (client-report-failed-chat-message app chat-message (client-chat-hud app)))))


(define-simple-class FabricChat (ChatBox)
  ((*init* screen :: Screen id :: String position :: Vector2f size :: Vector2f)
   (invoke-special ChatBox (this) '*init* screen id position size))
  ((onSendMsg msg::String) (let* ((chatfield (*:getChildElementById (this) "chatbox:ChatInput"))
                                  (screen (*:getScreen (this)))
                                  (app (*:getApplication screen))
                                  (chat-message (ChatMessage)))
                             (set-message-name! chat-message (player-namestring (client-player app)))
                             (set-message-contents! chat-message msg)
                             (set-message-reliable! chat-message #t)
                             (send-chat-message app chat-message)
                             (*:resetTabFocus chatfield))))

(define (init-hud app ::SimpleApplication name-string)
  (let ((screen (Screen app))
        (key-input ::KeyInput (client-key-input app)))
    (*:initialize screen)
    (*:addControl (client-gui-node app) screen)
    (let* ((settings (app-settings app))
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

      (*:setText nodeplate (string-capitalize (client-center-name app)))
      (*:setTextAlign nodeplate Align:Left)
      (*:setFont nodeplate "Interface/Fonts/Laconic24.fnt")
      (*:setFontSize nodeplate 24)
      (*:setFontColor nodeplate ColorRGBA:Green)

      (*:setFontColor chatlog ColorRGBA:Green)
      (*:removeEffect chatfield EffectEvent:TabFocus)
      (*:setFontSize chatfield 24)
      (*:setSendKey chatbox key-input:KEY_RETURN)
      
      (set-client-chat-hud! app chatbox)
      (*:addElement screen nameplate)
      (*:addElement screen nodeplate)
      (*:addElement screen chatbox))))


;;; ---------------------------------------------------------------------
;;; set up player controls
;;; ---------------------------------------------------------------------

(define (setup-inputs app ::SimpleApplication)
  ;; set up the player's controls
  (let ((key-input ::KeyInput (client-key-input app)))
    (*:addMapping (client-input-manager app) "moveForward" (KeyTrigger key-input:KEY_UP))
    (*:addMapping (client-input-manager app) "moveForward" (KeyTrigger key-input:KEY_W))
    (*:addMapping (client-input-manager app) "maybeMoveForward"
                   (MouseButtonTrigger MouseInput:BUTTON_LEFT))
    (*:addMapping (client-input-manager app) "leftButton"
                   (MouseButtonTrigger MouseInput:BUTTON_LEFT))
    (*:addMapping (client-input-manager app) "rightButton"
                   (MouseButtonTrigger MouseInput:BUTTON_RIGHT))
    (*:addMapping (client-input-manager app) "moveRight" (KeyTrigger key-input:KEY_RIGHT))
    (*:addMapping (client-input-manager app) "moveRight" (KeyTrigger key-input:KEY_D))
    (*:addMapping (client-input-manager app) "mouseRotateRight" (MouseAxisTrigger 0 #f))
    (*:addMapping (client-input-manager app) "moveLeft" (KeyTrigger key-input:KEY_LEFT))
    (*:addMapping (client-input-manager app) "moveLeft" (KeyTrigger key-input:KEY_A))
    (*:addMapping (client-input-manager app) "mouseRotateLeft" (MouseAxisTrigger 0 #t))
    (*:addMapping (client-input-manager app) "mouseRotateUp" (MouseAxisTrigger 1 #f))
    (*:addMapping (client-input-manager app) "moveBackward" (KeyTrigger key-input:KEY_DOWN))
    (*:addMapping (client-input-manager app) "moveBackward" (KeyTrigger key-input:KEY_S))
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

;;; ---------------------------------------------------------------------
;;; set up the scene
;;; ---------------------------------------------------------------------

(define (setup-lighting app ::SimpleApplication)
  (let* ((asset-manager::AssetManager (get-asset-manager))
         (bloom (BloomFilter BloomFilter:GlowMode:Objects))
         (filter-processor::FilterPostProcessor (FilterPostProcessor asset-manager)))
    (*:setDownSamplingFactor bloom 2.0)
    (*:setBloomIntensity bloom 2.0)
    (*:addFilter filter-processor bloom)
    (*:addProcessor (client-viewport app) filter-processor)))


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
      (set-center-name! app (choose-any (node-names))))
    (set! center-body (make-center-body app (client-center-name app)))
    (*:attachChild (client-root-node app) center-body)
    (init-player-character app)

    (let ((player (client-player app)))
      (init-hud app (player-namestring player)))
    (ensure-valid-network-client app)
    ;; uncomment to capture video to a file
    ;; (*:attach (client-state-manager app) (VideoRecorderAppState))
    #!void))


;;; ---------------------------------------------------------------------
;;; set up event-handling
;;; ---------------------------------------------------------------------

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

(define (handle-action-event app name key-pressed? tpf)
  (on-action (name)
             ("leftButton" -> (set-client-left-button! app key-pressed?))
             ("rightButton" -> (set-client-right-button! app key-pressed?))))


;;; ---------------------------------------------------------------------
;;; construct the client app
;;; ---------------------------------------------------------------------

;;; (make-client)
;;; ---------------------------------------------------------------------
;;; puts everything together into a runnable client

(define (make-client #!optional (center #f))
  (let* ((client :: FabricClient (FabricClient))
	 (settings :: AppSettings (app-settings client)))
    (when center
      (set-center-name! client center))
    (Serializer:registerClass ChatMessage)
    (*:setResolution settings 1920 1200)
    (*:setTitle settings "The Fabric")
    (*:setSettingsDialogImage settings "Interface/icon.jpg")
    (*:setSettings client settings)
    (*:setDisplayFps client #f) ; #t to show FPS
    (*:setShowSettings client #t) ; #t to show settings dialog
    (*:setDisplayStatView client #f) ; #t to show stats
    (*:setPauseOnLostFocus client #f)
    client))
