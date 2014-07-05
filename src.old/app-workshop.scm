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
(require "interface-consp.scm")
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
;;; FabricWorkshop - the workshop class
;;; ---------------------------------------------------------------------

(define-simple-class FabricWorkshop (FabricApp)

  ;; slots
  ;; -------
  (worker init-form: #!null)
  (worker-node :: Node init-form: #!null)
  (center-name ::String init-form: #!null)
  (direction ::Vector3f init-form: (Vector3f))
  (network-client ::com.jme3.network.Client  init-form: #!null)
  (left-button? init-form: #f)
  (right-button? init-form: #f)
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
  ((getWorker) worker)
  ((setWorker p) (set! worker p))
  ((getWorkerNode) worker-node)
  ((setWorkerNode n) (set! worker-node n))
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
  ((simpleInitApp)(init-workshop (this))))

;;; ---------------------------------------------------------------------
;;; FabricWorkshop accessors
;;; ---------------------------------------------------------------------

(defgetter (workshop-audio-renderer FabricWorkshop) getAudioRenderer)
(defgetter (workshop-camera FabricWorkshop) getCamera)
(defgetter (workshop-camera-direction FabricWorkshop) getCameraDirection)
(defgetter (workshop-center-name FabricWorkshop) getCenterName)
(defgetter (workshop-chat-hud FabricWorkshop) getChatHud)
(defgetter (workshop-direction FabricWorkshop) getDirection)
(defgetter (workshop-network-client FabricWorkshop) getNetworkClient)
(defgetter (workshop-fly-by-camera FabricWorkshop) getFlyByCamera)
(defgetter (workshop-gui-font FabricWorkshop) getGuiFont)
(defgetter (workshop-gui-node FabricWorkshop) getGuiNode)
(defgetter (workshop-input-manager FabricWorkshop) getInputManager)
(defgetter (workshop-key-input FabricWorkshop) getKeyInput)
(defgetter (workshop-left-button? FabricWorkshop) getLeftButton)
(defgetter (workshop-worker FabricWorkshop) getWorker)
(defgetter (workshop-worker-node FabricWorkshop) getWorkerNode)
(defgetter (workshop-right-button? FabricWorkshop) getRightButton)
(defgetter (workshop-root-node FabricWorkshop) getRootNode)
(defgetter (workshop-state-manager FabricWorkshop) getStateManager)
(defgetter (workshop-viewport FabricWorkshop) getViewport)

(defsetter (set-center-name! FabricWorkshop) setCenterName)
(defsetter (set-workshop-chat-hud! FabricWorkshop) setChatHud)
(defsetter (set-workshop-direction! FabricWorkshop) setDirection)
(defsetter (set-workshop-network-client! FabricWorkshop) setNetworkClient)
(defsetter (set-workshop-left-button! FabricWorkshop) setLeftButton)
(defsetter (set-workshop-worker! FabricWorkshop) setWorker)
(defsetter (set-workshop-worker-node! FabricWorkshop) setWorkerNode)
(defsetter (set-workshop-right-button! FabricWorkshop) setRightButton)

(define (workshop-camera-left app :: FabricWorkshop)
  (*:getLeft (workshop-camera app)))

(define (workshop-normalize-camera! app)
  (*:normalizeLocal (workshop-camera-direction app)))

;;; ---------------------------------------------------------------------
;;; set up the worker character
;;; ---------------------------------------------------------------------

;;; (assemble-worker-character pc-node pc-geom pc-controls)
;;; ---------------------------------------------------------------------
;;; assemble a worker-character's node, geometry, and controls

(define (assemble-worker-character pc-node pc-geom pc-controls)
  (*:attachChild pc-node pc-geom)
  (for-each (lambda (ctrl)
              (*:addControl pc-geom ctrl))
            pc-controls))

;;; (init-worker-camera app worker-node)
;;; ---------------------------------------------------------------------

(define (init-worker-camera app worker-node)
  (let* ((camera::com.jme3.renderer.Camera (workshop-camera app))
         (cam-node (CameraNode "camera" camera)))
    (*:setControlDir cam-node CameraControl:ControlDirection:SpatialToCamera)
    (*:setFrustumFar (workshop-camera app) 20000)
    ;; position the camera behind and above the worker and look at the worker
    (*:setLocalTranslation cam-node (Vector3f 0 30 -30))
    (*:lookAt cam-node (*:getLocalTranslation worker-node) Vector3f:UNIT_Y)
    ;; attach the camera to the worker character
    (*:attachChild worker-node cam-node)))


;;; (init-worker-character app ::SimpleApplication)
;;; ---------------------------------------------------------------------
;;; prepare a worker character and present it in the scene

(define (init-worker-character app ::SimpleApplication)
  (let* ((worker-node (Node "Worker"))
         (worker (make-worker-character (any-lit-color)))
         (worker-shape (get-key worker shape:))
         (worker-rotator (any-rotator)))
    (set-workshop-worker! app worker)
    (set-workshop-worker-node! app worker-node)
    ;; don't seize the mouse from the worker
    (Mouse:setGrabbed #f)
    ;; disable the fly-by camera
    (*:setEnabled (workshop-fly-by-camera app) #f)

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
    (*:attachChild (workshop-root-node app) worker-node)))

;;; ---------------------------------------------------------------------
;;; WorkshopChatHandler - aux class for handling incoming chat messages
;;; ---------------------------------------------------------------------

(define-simple-class WorkshopChatHandler (MessageListener)
  (application init-form: #!null)
  ((*init* app)(set! application app))
  ((messageReceived source msg) (if (instance? msg ChatMessage)
                                    (let* ((chatbox (workshop-chat-hud application))
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

(define (workshop-report-failed-chat-message app chat-message chat-box)
  (let* ((msg-name (message-name chat-message))
         (msg-contents (message-contents chat-message))
         (failed-text (format #f "Connection failed; unable to send message: [~A] ~A"
                              msg-name msg-contents)))
    (*:receiveMsg chat-box failed-text)))

(define (workshop-connect-to-server app)
  (try-catch
   (let ((new-connection (Network:connectToServer (server-name)(server-version)(server-host)
                                                  (server-port)(server-port))))
     (set-workshop-network-client! app new-connection)
     (*:addMessageListener (workshop-network-client app) (WorkshopChatHandler app))
     (*:start (workshop-network-client app)))
   (ex java.net.ConnectException (begin (set-workshop-network-client! app #!null)
                                        (format #t "~%Failed to connect to Fabric server.")
                                        (format #t "~%~A" (*:toString ex))))))

(define (ensure-valid-network-client app)
  (let ((net-client #f)
        (found-client (workshop-network-client app)))
    (when (jnull? found-client)
      (workshop-connect-to-server app))
    (set! net-client (workshop-network-client app))
    (if (jnull? net-client)
        #f
        (if (*:isConnected net-client)
            net-client
            #f))))

(define (send-chat-message app chat-message)
  (let ((net-client (ensure-valid-network-client app)))
    (if net-client
        (*:send net-client chat-message)
        (workshop-report-failed-chat-message app chat-message (workshop-chat-hud app)))))


(define-simple-class FabricChat (ChatBox)
  ((*init* screen :: Screen id :: String position :: Vector2f size :: Vector2f)
   (invoke-special ChatBox (this) '*init* screen id position size))
  ((onSendMsg msg::String) (let* ((chatfield (*:getChildElementById (this) "chatbox:ChatInput"))
                                  (screen (*:getScreen (this)))
                                  (app (*:getApplication screen))
                                  (chat-message (ChatMessage)))
                             (set-message-name! chat-message (worker-namestring (workshop-worker app)))
                             (set-message-contents! chat-message msg)
                             (set-message-reliable! chat-message #t)
                             (send-chat-message app chat-message)
                             (*:resetTabFocus chatfield))))

(define (init-hud app ::SimpleApplication name-string)
  (let ((screen (Screen app))
        (key-input ::KeyInput (workshop-key-input app)))
    (*:initialize screen)
    (*:addControl (workshop-gui-node app) screen)
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

      (*:setText nodeplate (string-capitalize (workshop-center-name app)))
      (*:setTextAlign nodeplate Align:Left)
      (*:setFont nodeplate "Interface/Fonts/Laconic24.fnt")
      (*:setFontSize nodeplate 24)
      (*:setFontColor nodeplate ColorRGBA:Green)

      (*:setFontColor chatlog ColorRGBA:Green)
      (*:removeEffect chatfield EffectEvent:TabFocus)
      (*:setFontSize chatfield 24)
      (*:setSendKey chatbox key-input:KEY_RETURN)
      
      (set-workshop-chat-hud! app chatbox)
      (*:addElement screen nameplate)
      (*:addElement screen nodeplate)
      (*:addElement screen chatbox))))


;;; ---------------------------------------------------------------------
;;; set up worker controls
;;; ---------------------------------------------------------------------

(define (setup-inputs app ::SimpleApplication)
  ;; set up the worker's controls
  (let ((key-input ::KeyInput (workshop-key-input app)))
    (*:addMapping (workshop-input-manager app) "moveForward" (KeyTrigger key-input:KEY_UP))
    (*:addMapping (workshop-input-manager app) "moveForward" (KeyTrigger key-input:KEY_W))
    (*:addMapping (workshop-input-manager app) "maybeMoveForward"
                   (MouseButtonTrigger MouseInput:BUTTON_LEFT))
    (*:addMapping (workshop-input-manager app) "leftButton"
                   (MouseButtonTrigger MouseInput:BUTTON_LEFT))
    (*:addMapping (workshop-input-manager app) "rightButton"
                   (MouseButtonTrigger MouseInput:BUTTON_RIGHT))
    (*:addMapping (workshop-input-manager app) "moveRight" (KeyTrigger key-input:KEY_RIGHT))
    (*:addMapping (workshop-input-manager app) "moveRight" (KeyTrigger key-input:KEY_D))
    (*:addMapping (workshop-input-manager app) "mouseRotateRight" (MouseAxisTrigger 0 #f))
    (*:addMapping (workshop-input-manager app) "moveLeft" (KeyTrigger key-input:KEY_LEFT))
    (*:addMapping (workshop-input-manager app) "moveLeft" (KeyTrigger key-input:KEY_A))
    (*:addMapping (workshop-input-manager app) "mouseRotateLeft" (MouseAxisTrigger 0 #t))
    (*:addMapping (workshop-input-manager app) "mouseRotateUp" (MouseAxisTrigger 1 #f))
    (*:addMapping (workshop-input-manager app) "moveBackward" (KeyTrigger key-input:KEY_DOWN))
    (*:addMapping (workshop-input-manager app) "moveBackward" (KeyTrigger key-input:KEY_S))
    (*:addMapping (workshop-input-manager app) "mouseRotateDown" (MouseAxisTrigger 1 #t))

       ;;; text inputs
    (*:addMapping (workshop-input-manager app) "SPACE" (KeyTrigger key-input:KEY_SPACE))
    (*:addMapping (workshop-input-manager app) "KEY_A" (KeyTrigger key-input:KEY_A))


    (*:addListener (workshop-input-manager app) app
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
    (*:addProcessor (workshop-viewport app) filter-processor)))


;;; (init-workshop app)
;;; ---------------------------------------------------------------------
;;; set up the scene and add the worker character

(define (init-workshop app)
  (let* ((sky (make-workshop-sky app))
         (center-body #f))

    (setup-lighting app)
    (setup-inputs app)
    (*:attachChild (workshop-root-node app) sky)
    (when (eq? #!null (workshop-center-name app))
      (set-center-name! app (choose-any (node-names))))
    (set! center-body (make-center-body app (workshop-center-name app)))
    (*:attachChild (workshop-root-node app) center-body)
    (init-worker-character app)

    (let ((worker (workshop-worker app)))
      (init-hud app (worker-namestring worker)))
    (ensure-valid-network-client app)
    ;; uncomment to capture video to a file
    ;; (*:attach (workshop-state-manager app) (VideoRecorderAppState))
    #!void))


;;; ---------------------------------------------------------------------
;;; set up event-handling
;;; ---------------------------------------------------------------------

;;; (handle-analog-event app name value tpf)
;;; ---------------------------------------------------------------------
;;; handle mouse movements and other continuous events

(define (handle-analog-event app name value tpf)
  (on-analog (name)
             ("moveForward" -> (begin (workshop-normalize-camera! app)
                                      (set-workshop-direction! app (workshop-camera-direction app))
                                      (*:multLocal (workshop-direction app) (* 800 tpf))
                                      (*:move (workshop-worker-node app) (workshop-direction app))))
             ("maybeMoveForward" -> (when (workshop-right-button? app)
                                      (workshop-normalize-camera! app)
                                      (set-workshop-direction! app (workshop-camera-direction app))
                                      (*:multLocal (workshop-direction app) (* 800 tpf))
                                      (*:move (workshop-worker-node app) (workshop-direction app))))
             ("moveBackward" -> (begin (workshop-normalize-camera! app)
                                       (set-workshop-direction! app (workshop-camera-direction app))
                                       (*:multLocal (workshop-direction app) (* -600 tpf))
                                       (*:move (workshop-worker-node app) (workshop-direction app))))
             ("moveRight" -> (begin (set-workshop-direction! app (*:normalizeLocal (*:getLeft (workshop-camera app))))
                                    (*:multLocal (workshop-direction app) (* -600 tpf))
                                    (*:move (workshop-worker-node app) (workshop-direction app))))
             ("moveLeft" -> (begin (set-workshop-direction! app (*:normalizeLocal (*:getLeft (workshop-camera app))))
                                   (*:multLocal (workshop-direction app) (* 600 tpf))
                                   (*:move (workshop-worker-node app) (workshop-direction app))))
             ("rotateRight" -> (*:rotate (workshop-worker-node app) 0 (* -0.25 tpf) 0))
             ("mouseRotateRight" -> (when (workshop-right-button? app)
                                      (*:rotate (workshop-worker-node app) 0 (* -1 value) 0)))
             ("rotateLeft" -> (*:rotate (workshop-worker-node app) 0 (* 0.25 tpf) 0))
             ("mouseRotateLeft" -> (when (workshop-right-button? app)
                                     (*:rotate (workshop-worker-node app) 0 (* 1 value) 0)))
             ("rotateUp" -> (*:rotate (workshop-worker-node app) (* -0.125 tpf) 0 0))
             ("mouseRotateUp" -> (when (workshop-right-button? app)
                                   (*:rotate (workshop-worker-node app) (* -1 value) 0 0)))
             ("rotateDown" -> (*:rotate (workshop-worker-node app) (* 0.125 tpf) 0 0))
             ("mouseRotateDown" -> (when (workshop-right-button? app)
                                     (*:rotate (workshop-worker-node app) (* 1 value) 0 0)))))

;;; (handle-action-event app name key-pressed? tpf)
;;; ---------------------------------------------------------------------
;;; handle keypresses, mouse clicks, and other discrete events

(define (handle-action-event app name key-pressed? tpf)
  (on-action (name)
             ("leftButton" -> (set-workshop-left-button! app key-pressed?))
             ("rightButton" -> (set-workshop-right-button! app key-pressed?))))

;;; ---------------------------------------------------------------------
;;; construct the workshop app
;;; ---------------------------------------------------------------------

;;; (make-workshop)
;;; ---------------------------------------------------------------------
;;; puts everything together into a runnable workshop

(define (make-workshop #!optional (center #f))
  (let* ((workshop :: FabricWorkshop (FabricWorkshop))
	 (settings :: AppSettings (app-settings workshop)))
    (when center
      (set-center-name! workshop center))
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