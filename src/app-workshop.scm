;;; ***********************************************************************
;;;;
;;;; Name:          app-workshop.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       a client for building game content
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export make-workshop)

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
(require "net-messaging.scm")
(require "util-lists.scm")
(require "view-shapes.scm")
(require "view-controls.scm")
(require "view-colors.scm")
(require "view-plasma.scm")
(require "view-worker.scm")
(require "init-config.scm")
(require "view-node.scm")
(require "syntax-events.scm")
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
(import-as ChatBox tonegod.gui.controls.extras.ChatBox)
(import-as ColorRGBA com.jme3.math.ColorRGBA)
(import-as ConnectException java.net.ConnectException)
(import-as EffectEvent tonegod.gui.effects.Effect:EffectEvent)
(import-as FilterPostProcessor com.jme3.post.FilterPostProcessor)
(import-as KeyInput com.jme3.input.KeyInput)
(import-as KeyTrigger com.jme3.input.controls.KeyTrigger)
(import-as TLabel tonegod.gui.controls.text.Label)
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
(import-as Vector2f com.jme3.math.Vector2f)
(import-as Vector3f com.jme3.math.Vector3f)
(import-as VideoRecorderAppState com.jme3.app.state.VideoRecorderAppState)

;;; ---------------------------------------------------------------------
;;; FabricWorkshop - the workshop class
;;; ---------------------------------------------------------------------

(defclass FabricWorkshop (FabricApp)
  (slots:
   (worker init-form: #!null getter: getWorker setter: setWorker)
   (worker-node :: Node init-form: #!null getter: getWorkerNode setter: setWorkerNode)
   (center-name ::String init-form: #!null getter: getCenterName setter: setCenterName)
   (direction ::Vector3f init-form: (Vector3f) getter: getDirection setter: setDirection)
   (network-client ::com.jme3.network.Client init-form: #!null getter: getNetworkClient setter: setNetworkClient)
   (left-button? init-form: #f getter: getLeftButton setter: setLeftButton)
   (right-button? init-form: #f getter: getRightButton setter: setRightButton)
   (chat-hud init-form: #!null getter: getChatHud setter: setChatHud))

  (methods:
   ((getCameraDirection) (*:getDirection cam))
   ((getAudioRenderer) audioRenderer)
   ((getViewport) viewPort)
   ((getInputManager) inputManager)
   ((getStateManager) stateManager)
   ((getKeyInput) keyInput)
   ((getGuiFont) guiFont)
   ((getGuiNode) guiNode)
   ((onAnalog name value tpf)(handle-analog-event (this) name value tpf))
   ((onAction name key-pressed? tpf)(handle-action-event (this) name key-pressed? tpf))
   ((simpleInitApp)(init-workshop (this)))))

(define (workshop-camera-left app :: FabricWorkshop)
  (*:getLeft (*:getCamera app)))

(define (workshop-normalize-camera! app)
  (*:normalizeLocal (*:getDirection (*:getCamera app))))

;;; ---------------------------------------------------------------------
;;; set up the worker character
;;; ---------------------------------------------------------------------

;;; assemble the worker
;;; ---------------------------------------------------------------------

(define (assemble-worker-character pc-node pc-geom pc-controls)
  (*:attachChild pc-node pc-geom)
  (for-each (lambda (ctrl)
              (*:addControl pc-geom ctrl))
            pc-controls))

;;; set up the worker's camera
;;; ---------------------------------------------------------------------

(define (init-worker-camera app worker-node)
  (let* ((camera::com.jme3.renderer.Camera (*:getCamera app))
         (cam-node (CameraNode "camera" camera)))
    (*:setControlDir cam-node CameraControl:ControlDirection:SpatialToCamera)
    (*:setFrustumFar (*:getCamera app) 20000)
    ;; position the camera behind and above the worker and look at the worker
    (*:setLocalTranslation cam-node (Vector3f 0 30 -30))
    (*:lookAt cam-node (*:getLocalTranslation worker-node) Vector3f:UNIT_Y)
    ;; attach the camera to the worker character
    (*:attachChild worker-node cam-node)))

;;; prepare a worker character and present it in the scene
;;; ---------------------------------------------------------------------

(define (init-worker-character app ::SimpleApplication)
  (let* ((worker-node (Node "Worker"))
         (worker (make-worker-character (any-lit-color)))
         (worker-shape (get-key worker shape:))
         (worker-rotator (any-rotator)))
    (*:setWorker app worker)
    (*:setWorkerNode app worker-node)
    ;; don't seize the mouse from the worker
    (Mouse:setGrabbed #f)
    ;; disable the fly-by camera
    (*:setEnabled (*:getFlyByCamera app) #f)

    ;; assemble the worker character's parts
    (assemble-worker-character worker-node worker-shape (list worker-rotator))
    
    ;; set up the worker character's camera
    (init-worker-camera app worker-node)

    ;; move the character to its starting location and point it at the center
    (*:setLocalTranslation worker-node 0.0 8000.0 0.0)
    (let ((rotation (Quaternion))
          (pitch-axis (Vector3f 1 0 0)))
      ;; PI/4 radians points us right at the center
      (*:fromAngleAxis rotation (/ PI 4) pitch-axis)
      (*:setLocalRotation worker-node rotation))
    
    ;; add the worker to the scene
    (*:attachChild (*:getRootNode app) worker-node)))

;;; ---------------------------------------------------------------------
;;; the chatbox
;;; ---------------------------------------------------------------------

;;; aux class for handling incoming chat messages
;;; ---------------------------------------------------------------------

(defclass WorkshopChatHandler (MessageListener)
  (slots: (application init-form: #!null))
  (methods:
   ((*init* app)(set! application app))
   ((messageReceived source msg)
    (if (instance? msg ChatMessage)
        (let* ((chatbox (*:getChatHud application))
               (msg-name (*:getName msg))
               (msg-contents (*:getContents msg))
               (received-text (format #f "[~A] ~A" msg-name msg-contents))
               (updater (runnable (lambda ()
                                    (*:receiveMsg chatbox received-text)))))
          (*:enqueue application updater))
        (format #t "Unrecognized message: ~s" msg)))))


;;; helper functions
;;; ---------------------------------------------------------------------

(define (workshop-report-failed-chat-message app chat-message chat-box)
  (let* ((msg-name (*:getName chat-message))
         (msg-contents (*:getContents chat-message))
         (failed-text (format #f "Connection failed; unable to send message: [~A] ~A"
                              msg-name msg-contents)))
    (*:receiveMsg chat-box failed-text)))

(define (workshop-connect-to-server app)
  (try-catch
   (let ((new-connection (Network:connectToServer (server-name)(server-version)(server-host)
                                                  (server-port)(server-port))))
     (*:setNetworkClient app new-connection)
     (*:addMessageListener (*:getNetworkClient app) (WorkshopChatHandler app))
     (*:start (*:getNetworkClient app)))
   (ex java.net.ConnectException (begin (*:setNetworkClient app #!null)
                                        (format #t "~%Failed to connect to Fabric server.")
                                        (format #t "~%~A" (*:toString ex))))))

(define (ensure-valid-network-client app)
  (let ((net-client #f)
        (found-client (*:getNetworkClient app)))
    (when (jnull? found-client)
      (workshop-connect-to-server app))
    (set! net-client (*:getNetworkClient app))
    (if (jnull? net-client)
        #f
        (if (*:isConnected net-client)
            net-client
            #f))))

(define (send-chat-message app chat-message)
  (let ((net-client (ensure-valid-network-client app)))
    (if net-client
        (*:send net-client chat-message)
        (workshop-report-failed-chat-message app chat-message (*:getChatHud app)))))

;;; the chatboc class
;;; ---------------------------------------------------------------------

(defclass FabricChat (ChatBox)
  (methods:
   ((*init* screen :: Screen id :: String position :: Vector2f size :: Vector2f)
    (invoke-special ChatBox (this) '*init* screen id position size))
   ((onSendMsg msg::String)
    (let* ((chatfield (*:getChildElementById (this) "chatbox:ChatInput"))
           (screen (*:getScreen (this)))
           (app (*:getApplication screen))
           (chat-message (ChatMessage)))
      (*:setName chat-message (worker-namestring (*:getWorker app)))
      (*:setContents chat-message msg)
      (*:setReliable chat-message #t)
      (send-chat-message app chat-message)
      (*:resetTabFocus chatfield)))))

;;; set up the HUD
;;; ---------------------------------------------------------------------

(define (init-hud app ::SimpleApplication name-string)
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
      
      (*:setChatHud app chatbox)
      (*:addElement screen nameplate)
      (*:addElement screen nodeplate)
      (*:addElement screen chatbox))))

;;; ---------------------------------------------------------------------
;;; worker inputs
;;; ---------------------------------------------------------------------

(define (setup-inputs app ::SimpleApplication)
  ;; set up the worker's controls
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

    ;; set up event listener
    (*:addListener input-manager app
                   ;; motion controls
                   "moveForward" "maybeMoveForward" "moveBackward" "moveRight" "moveLeft"
                   "leftButton" "rightButton" "rotateRight" "rotateLeft" "rotateUp" "rotateDown"
                   "mouseRotateRight" "mouseRotateLeft" "mouseRotateUp" "mouseRotateDown"
                   ;; chat input
                   "SPACE" "KEY_A")))

;;; ---------------------------------------------------------------------
;;; scene setup
;;; ---------------------------------------------------------------------

;;; lighting
;;; ---------------------------------------------------------------------

(define (setup-lighting app ::SimpleApplication)
  (let* ((asset-manager::AssetManager (get-asset-manager))
         (bloom (BloomFilter BloomFilter:GlowMode:Objects))
         (filter-processor::FilterPostProcessor (FilterPostProcessor asset-manager)))
    (*:setDownSamplingFactor bloom 2.0)
    (*:setBloomIntensity bloom 2.0)
    (*:addFilter filter-processor bloom)
    (*:addProcessor (*:getViewport app) filter-processor)))

;;; present the worker
;;; ---------------------------------------------------------------------

(define (init-workshop app)
  (let* ((sky (make-workshop-sky app))
         (center-body #f))

    (setup-lighting app)
    (setup-inputs app)
    (*:attachChild (*:getRootNode app) sky)
    (when (eq? #!null (*:getCenterName app))
      (*:setCenterName app (choose-any (node-names))))
    (set! center-body (make-center-body app (*:getCenterName app)))
    (*:attachChild (*:getRootNode app) center-body)
    (init-worker-character app)

    (let ((worker (*:getWorker app)))
      (init-hud app (worker-namestring worker)))
    (ensure-valid-network-client app)
    ;; uncomment to capture video to a file
    ;; (*:attach (workshop-state-manager app) (VideoRecorderAppState))
    #!void))

;;; ---------------------------------------------------------------------
;;; set up event-handling
;;; ---------------------------------------------------------------------

;;; handle mouse movements and other continuous events
;;; ---------------------------------------------------------------------

(define (handle-analog-event app name value tpf)
  (on-analog (name)
             ("moveForward"
              -> (begin (workshop-normalize-camera! app)
                        (*:setDirection app (*:getCameraDirection app))
                        (*:multLocal (*:getDirection app) (* 300 tpf))
                        (*:move (*:getWorkerNode app) (*:getDirection app))))
             ("maybeMoveForward"
              -> (when (*:getRightButton app)
                   (workshop-normalize-camera! app)
                   (*:setDirection app (*:getCameraDirection app))
                   (*:multLocal (*:getDirection app) (* 300 tpf))
                   (*:move (*:getWorkerNode app) (*:getDirection app))))
             ("moveBackward"
              -> (begin (workshop-normalize-camera! app)
                        (*:setDirection app (*:getCameraDirection app))
                        (*:multLocal (*:getDirection app) (* -200 tpf))
                        (*:move (*:getWorkerNode app) (*:getDirection app))))
             ("moveRight"
              -> (begin (*:setDirection app (*:normalizeLocal (*:getLeft (*:getCamera app))))
                        (*:multLocal (*:getDirection app) (* -150 tpf))
                        (*:move (*:getWorkerNode app) (*:getDirection app))))
             ("moveLeft"
              -> (begin (*:setDirection app (*:normalizeLocal (*:getLeft (*:getCamera app))))
                        (*:multLocal (*:getDirection app) (* 150 tpf))
                        (*:move (*:getWorkerNode app) (*:getDirection app))))
             ("rotateRight"
              -> (*:rotate (*:getWorkerNode app) 0 (* -0.25 tpf) 0))
             ("mouseRotateRight"
              -> (when (*:getRightButton app)
                   (*:rotate (*:getWorkerNode app) 0 (* -1 value) 0)))
             ("rotateLeft"
              -> (*:rotate (*:getWorkerNode app) 0 (* 0.25 tpf) 0))
             ("mouseRotateLeft"
              -> (when (*:getRightButton app)
                   (*:rotate (*:getWorkerNode app) 0 (* 1 value) 0)))
             ("rotateUp"
              -> (*:rotate (*:getWorkerNode app) (* -0.125 tpf) 0 0))
             ("mouseRotateUp"
              -> (when (*:getRightButton app)
                   (*:rotate (*:getWorkerNode app) (* -1 value) 0 0)))
             ("rotateDown"
              -> (*:rotate (*:getWorkerNode app) (* 0.125 tpf) 0 0))
             ("mouseRotateDown"
              -> (when (*:getRightButton app)
                   (*:rotate (*:getWorkerNode app) (* 1 value) 0 0)))))

;;; handle keypresses, mouse clicks, and other discrete events
;;; ---------------------------------------------------------------------

(define (handle-action-event app name key-pressed? tpf)
  (on-action (name)
             ("leftButton" -> (*:setLeftButton app key-pressed?))
             ("rightButton" -> (*:setRightButton app key-pressed?))))

;;; ---------------------------------------------------------------------
;;; construct the workshop app
;;; ---------------------------------------------------------------------

(define (make-workshop #!optional (center #f))
  (let* ((workshop :: FabricWorkshop (FabricWorkshop))
	 (settings :: AppSettings (*:getAppSettings workshop)))
    (when center
      (*:setCenterName workshop center))
    (Serializer:registerClass ChatMessage)
    (*:setResolution settings 1920 1200)
    (*:setTitle settings "The Fabric")
    (*:setSettingsDialogImage settings "Interface/icon.jpg")
    (*:setSettings workshop settings)
    (*:setDisplayFps workshop #f) ; #t to show FPS
    (*:setShowSettings workshop #t) ; #t to show settings dialog
    (*:setDisplayStatView workshop #f) ; #t to show stats
    (*:setPauseOnLostFocus workshop #f)
    workshop))
