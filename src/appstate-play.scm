;;;; ***********************************************************************
;;;;
;;;; Name:          appstate-play.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       play the game
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export )

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; 

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "syntax-classes.scm")
(require "syntax-events.scm")
(require "assets-general.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as AbstractAppState com.jme3.app.state.AbstractAppState)

;;; ---------------------------------------------------------------------
;;; the PlayAppState class
;;; ---------------------------------------------------------------------

(defclass PlayAppState (AbstractAppState)
  (slots:
   (direction type: Vector3f init-form: (Vector3f) getter: getDirection setter: setDirection)
   (speed type: float init-form: 0.0 getter: getSpeed setter: setSpeed)
   (center-name ::String init-form: #!null getter: getCenterName setter: setCenterName)
   (left-button? init-form: #f getter: getLeftButton setter: setLeftButton)
   (right-button? init-form: #f getter: getRightButton setter: setRightButton)
   (chat-hud init-form: #!null getter: getChatHud setter: setChatHud)
   (video-capture? init-form: #f getter: isVideoCapture setter: setVideoCapture)
   (player init-form: #!null getter: getPlayer setter: setPlayer)
   (player-node type: Node init-form: #!null getter: getPlayerNode setter: setPlayerNode))
  (methods:
   ((onAnalog name value tpf)(handle-analog-event (this) name value tpf))
   ((onAction name key-pressed? tpf)(handle-action-event (this) name key-pressed? tpf)))
  (init: (when (*:isVideoCapture (this))
           (*:attach (*:getStateManager (this))
                     (VideoRecorderAppState)))))

;;; =====================================================================
;;; code harvested from client-main.scm
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; chatbox
;;; ---------------------------------------------------------------------


;;; aux class for handling incoming chat messages
;;; ---------------------------------------------------------------------

(defclass ClientChatHandler (MessageListener)
  (slots: (application type: FabricClient init-form: #!null))
  (methods:
   ((*init* app)(set! application app))
   ((messageReceived source::DefaultClient msg::ChatMessage)
    (if (instance? msg ChatMessage)
        (let* ((chatbox::ChatBox (*:getChatHud application))
               (msg-name (*:getName msg))
               (msg-contents (*:getContents msg))
               (received-text (format #f "[~A] ~A" msg-name msg-contents))
               (updater (runnable (lambda ()(*:receiveMsg chatbox received-text)))))
          (*:enqueue application updater))
        (warn "unrecognized message: ~s" msg)))))


;;; helper functions
;;; ---------------------------------------------------------------------

(define (report-failed-chat-message app::FabricClient chat-message::ChatMessage chat-box::ChatBox)
  (let* ((msg-name (*:getName chat-message))
         (msg-contents (*:getContents chat-message))
         (failed-text (format #f "Warning: connection failed; unable to send message: [~A] ~A"
                              msg-name msg-contents)))
    (*:receiveMsg chat-box failed-text)))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (send-chat-message app::FabricClient chat-message)
  (let ((net-client::Client (ensure-valid-network-client app)))
    (if (jnull? net-client)
        (report-failed-chat-message app chat-message (*:getChatHud app))
        (*:send net-client chat-message))))

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

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (normalize-camera! app :: FabricClient)
  (let ((dir :: Vector3f (*:getCameraDirection app)))
    (*:normalizeLocal dir)))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (move-player!  app :: FabricClient node :: Node amount :: float invert?)
  (let ((dir :: Vector3f (*:getDirection app))
        (sign (if invert? -1 1)))
    (*:multLocal dir (* sign amount))
    (*:move node dir)))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (move-player-forward! app :: FabricClient node :: Node amount :: float)
  (normalize-camera! app)
  (*:setDirection app (*:getCameraDirection app))
  (move-player! app node amount #f))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (move-player-backward! app :: FabricClient node :: Node amount :: float)
  (normalize-camera! app)
  (*:setDirection app (*:getCameraDirection app))
  (move-player! app node amount #t))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (move-player-left! app :: FabricClient node :: Node amount :: float)
  (*:setDirection app (*:normalizeLocal (*:getLeft (*:getCamera app))))
  (move-player! app node amount #f))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (move-player-right! app :: FabricClient node :: Node amount :: float)
  (*:setDirection app (*:normalizeLocal (*:getLeft (*:getCamera app))))
  (move-player! app node amount #t))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (rotate-player-right! node :: Node amount :: float)
  (*:rotate node 0 (* -1 amount) 0))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (rotate-player-left! node :: Node amount :: float)
  (*:rotate node 0 amount 0))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (rotate-player-up! node :: Node amount :: float)
  (*:rotate node (* -1 amount) 0 0))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (rotate-player-down! node :: Node amount :: float)
  (*:rotate node amount 0 0))

;;; ---------------------------------------------------------------------
;;; set up player inputs
;;; ---------------------------------------------------------------------

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (setup-inputs app::FabricClient)
  ;; set up the player's controls
  (let ((key-input ::KeyInput (*:getKeyInput app))
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

(define (init-ui app::FabricClient)
  (let ((screen::Screen (Screen app))
        (key-input::KeyInput (*:getKeyInput app)))
    (*:initialize screen)
    (*:addControl (*:getGuiNode app) screen)
    (let* ((settings::AppSettings (*:getAppSettings app))
           (Align BitmapFont:Align)
           (VAlign BitmapFont:VAlign)
           (width (*:getWidth settings))
           (height (*:getHeight settings))
           (chatbox (FabricChat screen "chatbox"
                                (Vector2f 15 (- height 220))
                                (Vector2f 400 200)))
           (chatfield (*:getChildElementById chatbox "chatbox:ChatInput"))
           (chatlog (*:getChildElementById chatbox "chatbox:ChatArea")))

      (*:setFontColor chatlog ColorRGBA:Green)
      (*:removeEffect chatfield EffectEvent:TabFocus)
      (*:setFontSize chatfield 24)
      (*:setSendKey chatbox key-input:KEY_RETURN)
      
      (*:setChatHud app chatbox)
      (*:addElement screen chatbox))))


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


;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (init-play-app-state app::FabricClient)
  ;; set up the scene
  (setup-lighting app)
  (setup-inputs app)
  ;; set up connectivity
  (ensure-valid-network-client app)
  ;; set up the UI
  (init-ui app)
  ;; don't seize the mouse from the player
  (Mouse:setGrabbed #f)
  ;; disable the fly-by camera
  (*:setEnabled (*:getFlyByCamera app) #f)
  ;; initialize play-time parameters
  (*:setSpeed app 300.0)
  ;; return void to make Java happy
  #!void)
