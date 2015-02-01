;;;; ***********************************************************************
;;;;
;;;; Name:          client-main.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       fabric client main program
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export FabricClient make-client
               move-player-forward! move-player-backward!
               move-player-left! move-player-right!
               normalize-camera!
               rotate-player-left! rotate-player-right!
               rotate-player-up! rotate-player-down!)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "net-messaging.scm")
(require "syntax-classes.scm")
(require "syntax-events.scm")
(require "assets-general.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as ActionListener com.jme3.input.controls.ActionListener)
(import-as AnalogListener com.jme3.input.controls.AnalogListener)
(import-as AppSettings com.jme3.system.AppSettings)
(import-as AssetManager com.jme3.asset.AssetManager)
(import-as BitmapFont com.jme3.font.BitmapFont)
(import-as BloomFilter com.jme3.post.filters.BloomFilter)
(import-as CameraControl com.jme3.scene.control.CameraControl)
(import-as CameraNode com.jme3.scene.CameraNode)
(import-as ChatBox tonegod.gui.controls.extras.ChatBox)
(import-as Client com.jme3.network.Client)
(import-as ColorRGBA com.jme3.math.ColorRGBA)
(import-as ConnectException java.net.ConnectException)
(import-as EffectEvent tonegod.gui.effects.Effect:EffectEvent)
(import-as FilterPostProcessor com.jme3.post.FilterPostProcessor)
(import-as KeyInput com.jme3.input.KeyInput)
(import-as KeyTrigger com.jme3.input.controls.KeyTrigger)
(import-as MessageListener com.jme3.network.MessageListener)
(import-as Mouse org.lwjgl.input.Mouse)
(import-as MouseAxisTrigger com.jme3.input.controls.MouseAxisTrigger)
(import-as MouseButtonTrigger com.jme3.input.controls.MouseButtonTrigger)
(import-as MouseInput com.jme3.input.MouseInput)
(import-as Network com.jme3.network.Network)
(import-as Node com.jme3.scene.Node)
(import-as PI com.jme3.math.FastMath:PI)
(import-as Quaternion com.jme3.math.Quaternion)
(import-as Screen tonegod.gui.core.Screen)
(import-as Serializer com.jme3.network.serializing.Serializer)
(import-as SimpleApplication com.jme3.app.SimpleApplication)
(import-as String java.lang.String)
(import-as TextField tonegod.gui.controls.text.TextField)
(import-as TLabel tonegod.gui.controls.text.Label)
(import-as Vector2f com.jme3.math.Vector2f)
(import-as Vector3f com.jme3.math.Vector3f)
(import-as VideoRecorderAppState com.jme3.app.state.VideoRecorderAppState)
(import-as ViewPort com.jme3.renderer.ViewPort)

;;; ---------------------------------------------------------------------
;;; FabricClient - the client application class
;;; ---------------------------------------------------------------------

(defclass FabricClient (SimpleApplication AnalogListener ActionListener)
  (slots:
   (app-settings init-form: (AppSettings #t) getter: getAppSettings)
   (direction type: Vector3f init-form: (Vector3f) getter: getDirection setter: setDirection)
   (speed type: float init-form: 0.0 getter: getSpeed setter: setSpeed)
   (network-client ::com.jme3.network.Client init-form: #!null getter: getNetworkClient setter: setNetworkClient)
   (center-name ::String init-form: #!null getter: getCenterName setter: setCenterName)
   (left-button? init-form: #f getter: getLeftButton setter: setLeftButton)
   (right-button? init-form: #f getter: getRightButton setter: setRightButton)
   (chat-hud init-form: #!null getter: getChatHud setter: setChatHud)
   (video-capture? init-form: #f getter: isVideoCapture setter: setVideoCapture)
   (player init-form: #!null getter: getPlayer setter: setPlayer)
   (player-node type: Node init-form: #!null getter: getPlayerNode setter: setPlayerNode))
  (methods:
   ((getCameraDirection) (*:getDirection cam))
   ((getAudioRenderer) audioRenderer)
   ((getViewport) viewPort)
   ((getInputManager) inputManager)
   ((getStateManager) stateManager)
   ((getGuiNode) guiNode)
   ((getGuiFont) guiFont)
   ((getKeyInput) keyInput)
   ((onAnalog name value tpf)(handle-analog-event (this) name value tpf))
   ((onAction name key-pressed? tpf)(handle-action-event (this) name key-pressed? tpf))
   ((simpleInitApp)(init-client (this))))
  (init: (when (*:isVideoCapture (this))
           (*:attach (*:getStateManager (this))
                     (VideoRecorderAppState)))))

;;; ---------------------------------------------------------------------
;;; chatbox
;;; ---------------------------------------------------------------------


;;; aux class for handling incoming chat messages
;;; ---------------------------------------------------------------------

(defclass ClientChatHandler (MessageListener)
  (slots: (application type: FabricClient init-form: #!null))
  (methods:
   ((*init* app)(set! application app))
   ((messageReceived source msg::ChatMessage)
    (if (instance? msg ChatMessage)
        (let* ((chatbox::ChatBox (*:getChatHud application))
               (msg-name (*:getName msg))
               (msg-contents (*:getContents msg))
               (received-text (format #f "[~A] ~A" msg-name msg-contents))
               (updater (runnable (lambda ()(*:receiveMsg chatbox received-text)))))
          (*:enqueue application updater))
        (format #t "Unrecognized message: ~s" msg)))))

;;; helper functions
;;; ---------------------------------------------------------------------

(define (report-failed-chat-message app::FabricClient chat-message::ChatMessage chat-box::ChatBox)
  (let* ((msg-name (*:getName chat-message))
         (msg-contents (*:getContents chat-message))
         (failed-text (format #f "Connection failed; unable to send message: [~A] ~A"
                              msg-name msg-contents)))
    (*:receiveMsg chat-box failed-text)))

(define (connect-to-server app::FabricClient)
  (try-catch
   (let ((new-connection (Network:connectToServer (server-name)(server-version)(server-host)
                                                  (server-port)(server-port))))
     (*:setNetworkClient app new-connection)
     (*:addMessageListener new-connection (ClientChatHandler app))
     (*:start new-connection))
   (ex ConnectException (begin (*:setNetworkClient app #!null)
                               (format #t "~%Failed to connect to Fabric server.")
                               (format #t "~%~A" (*:toString ex))))))

(define (ensure-valid-network-client app::FabricClient)
  (let ((net-client::Client #!null)
        (found-client::Client (*:getNetworkClient app)))
    (when (jnull? found-client)
      (connect-to-server app))
    (set! net-client (*:getNetworkClient app))
    (if (jnull? net-client)
        #f
        (if (*:isConnected net-client)
            net-client
            #f))))

(define (send-chat-message app::FabricClient chat-message)
  (let ((net-client::Client (ensure-valid-network-client app)))
    (if net-client
        (*:send net-client chat-message)
        (report-failed-chat-message app chat-message (*:getChatHud app)))))

;;; the chatbox class
;;; ---------------------------------------------------------------------

(defclass FabricChat (ChatBox)
  (slots: (chatname type: String init-form: "" getter: getChatName setter: setChatName))
  (methods:
   ((*init* screen :: Screen id :: String position :: Vector2f size :: Vector2f)
    (invoke-special ChatBox (this) '*init* screen id position size))
   ((onSendMsg msg :: String)
    (let* ((chatfield::TextField (*:getChildElementById (this) "chatbox:ChatInput"))
           (screen (*:getScreen (this)))
           (app::FabricClient (*:getApplication screen))
           (chat-message (ChatMessage)))
      (*:setName chat-message chatname)
      (*:setContents chat-message msg)
      (*:setReliable chat-message #t)
      (send-chat-message app chat-message)
      (*:resetTabFocus chatfield)))))

;;; ---------------------------------------------------------------------
;;; player movement
;;; ---------------------------------------------------------------------

(define (normalize-camera! app :: FabricClient)
  (let ((dir :: Vector3f (*:getCameraDirection app)))
    (*:normalizeLocal dir)))

(define (move-player!  app :: FabricClient node :: Node amount :: float invert?)
  (let ((dir :: Vector3f (*:getDirection app))
        (sign (if invert? -1 1)))
    (*:multLocal dir (* sign amount))
    (*:move node dir)))

(define (move-player-forward! app :: FabricClient node :: Node amount :: float)
  (normalize-camera! app)
  (*:setDirection app (*:getCameraDirection app))
  (move-player! app node amount #f))

(define (move-player-backward! app :: FabricClient node :: Node amount :: float)
  (normalize-camera! app)
  (*:setDirection app (*:getCameraDirection app))
  (move-player! app node amount #t))

(define (move-player-left! app :: FabricClient node :: Node amount :: float)
  (*:setDirection app (*:normalizeLocal (*:getLeft (*:getCamera app))))
  (move-player! app node amount #f))

(define (move-player-right! app :: FabricClient node :: Node amount :: float)
  (*:setDirection app (*:normalizeLocal (*:getLeft (*:getCamera app))))
  (move-player! app node amount #t))

(define (rotate-player-right! node :: Node amount :: float)
  (*:rotate node 0 (* -1 amount) 0))

(define (rotate-player-left! node :: Node amount :: float)
  (*:rotate node 0 amount 0))

(define (rotate-player-up! node :: Node amount :: float)
  (*:rotate node (* -1 amount) 0 0))

(define (rotate-player-down! node :: Node amount :: float)
  (*:rotate node amount 0 0))

;;; ---------------------------------------------------------------------
;;; set up player inputs
;;; ---------------------------------------------------------------------

(define (setup-inputs app::FabricClient)
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
;;; scene-setting
;;; ---------------------------------------------------------------------

;;; lighting
;;; ---------------------------------------------------------------------

(define (setup-lighting app::FabricClient)
  (let* ((asset-manager::AssetManager (get-asset-manager))
         (bloom (BloomFilter BloomFilter:GlowMode:Objects))
         (filter-processor::FilterPostProcessor (FilterPostProcessor asset-manager))
         (viewport::ViewPort (*:getViewport app)))
    (*:setDownSamplingFactor bloom 2.0)
    (*:setBloomIntensity bloom 2.0)
    (*:addFilter filter-processor bloom)
    (*:addProcessor viewport filter-processor)))


;;; initializing the app
;;; ---------------------------------------------------------------------

(define (init-client app::FabricClient)
  ;; set up the scene
  (setup-lighting app)
  (setup-inputs app)
  ;; set up connectivity
  (ensure-valid-network-client app)
  ;; set up the UI
  ;; don't seize the mouse from the player
  (Mouse:setGrabbed #f)
  ;; disable the fly-by camera
  (*:setEnabled (*:getFlyByCamera app) #f)
  ;; return void to make Java happy
  #!void)

;;; ---------------------------------------------------------------------
;;; event-handling
;;; ---------------------------------------------------------------------

;;; (handle-analog-event app name value tpf)
;;; ---------------------------------------------------------------------
;;; handle mouse movements and other continuous events

(define (handle-analog-event app::FabricClient name value tpf)
  (let ((speed (*:getSpeed app))
        (node (*:getPlayerNode app))
        (right-button-down? (*:getRightButton app)))
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

(define (handle-action-event app::FabricClient name key-pressed? tpf)
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

;;; (define $client (make-client))
;;; (*:start $client)
