;;;; ***********************************************************************
;;;;
;;;; Name:          client.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the Fabric client
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 FabricClient
 activate-create-character-state
 activate-login-state
 activate-pick-character-state
 activate-pick-location-state
 activate-play-state
 activate-transition-state
 client-handle-message
 current-character
 current-location
 current-user
 init-defaults
 make-client
 set-current-user!
 set-current-character!
 set-current-location!
 setup-inputs
 show
 start-client
 stop-client
 the-client)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require data-assets)
(require data-config)
(require data-users)
(require model-character)
(require model-rect)
(require model-user)
(require state)
(require state-login)
(require state-create-character)
(require state-pick-character)
(require state-pick-location)
(require state-play)
(require state-transition)
(require listener-message-client)
(require message)
(require message-activate-pick-character)
(require message-activate-create-character)
(require message-activate-pick-location)
(require message-activate-login)
(require message-activate-play)
(require view-client-error)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (only (com jme3 input MouseInput) AXIS_X AXIS_Y))

(import (class com.jme3.app SimpleApplication))
(import (class com.jme3.app.state AbstractAppState AppStateManager))
(import (class com.jme3.asset AssetManager))
(import (class com.jme3.input InputManager KeyInput MouseInput))
(import (class com.jme3.input.controls ActionListener AnalogListener
          KeyTrigger MouseAxisTrigger MouseButtonTrigger))
(import (class com.jme3.math Vector2f Vector3f))
(import (class com.jme3.network Client Message))
(import (class com.jme3.post FilterPostProcessor))
(import (class com.jme3.post.filters BloomFilter))
(import (class com.jme3.renderer ViewPort))
(import (class com.jme3.system AppSettings))
(import (class gnu.mapping Symbol))
(import (class java.lang String Thread))
(import (class org.lwjgl.input Mouse))
(import (class tonegod.gui.core Screen))

;;; ---------------------------------------------------------------------
;;; FabricClient
;;; ---------------------------------------------------------------------

(define the-client (make-parameter #!null))

(define-simple-class FabricClient (SimpleApplication AnalogListener ActionListener)
  ;; slots
  (current-user::FabricUser init: #!null)
  (current-character::FabricCharacter init: #!null)
  (current-location::String init: #!null)
  (username::String init: #!null)
  (password-hash::String init: #!null)
  (password-salt::byte[] init: #!null)
  (app-settings init: #!null)
  (state init: #!null)
  (screen init: #!null)
  (speed init: #!null)
  (direction type: Vector3f init-form: (Vector3f))
  (left-button? init-form: #f)
  (right-button? init-form: #f)
  (message-listener::FabricClientMessageListener init: #!null)
  ;; accessors
  ((getKeyInput) keyInput)
  ((getCameraDirection) (*:getDirection cam))
  ((getViewport) viewPort)
  ;; event handlers
  ((onAnalog name value tpf)(*:handleAnalogEvent (as FabricClientState state) name value tpf))
  ((onAction name key-pressed? tpf)(*:handleActionEvent (as FabricClientState state) name key-pressed? tpf))
  ;; init
  ((simpleInitApp) (init-client (this))))


(define (setup-lighting app::FabricClient)
  (let* ((asset-manager::AssetManager (get-asset-manager))
         (bloom (BloomFilter BloomFilter:GlowMode:Objects))
         (filter-processor::FilterPostProcessor (FilterPostProcessor asset-manager))
         (viewport::ViewPort (*:getViewport app)))
    (*:setDownSamplingFactor bloom 2.0)
    (*:setBloomIntensity bloom 2.0)
    (*:addFilter filter-processor bloom)
    (*:addProcessor viewport filter-processor)))

(define (init-client app::FabricClient)
  (begin (load-client-configuration)
         (set! app:message-listener (FabricClientMessageListener app))
         (*:setEnabled (*:getFlyByCamera app) #f)
         (set! app:screen (Screen app))
         (setup-inputs app)
         (setup-lighting app)
         (activate-transition-state)
         #!void))

(define (make-client #!key
                     (client::FabricClient (FabricClient))
                     (settings::AppSettings (AppSettings #t))
                     (screen-width 1920)
                     (screen-height 1200)
                     (title "The Fabric")
                     (settings-image "Interface/icon.jpg")
                     (show-fps #f)
                     (show-settings #t)
                     (show-statistics #f)
                     (pause-on-lost-focus #f)
                     (grab-mouse #f))
  (*:setResolution settings screen-width screen-height)
  (*:setTitle settings title)
  (*:setSettingsDialogImage settings settings-image)
  (*:setSettings client settings)
  (*:setDisplayFps client show-fps)
  (*:setShowSettings client show-settings)
  (*:setDisplayStatView client show-statistics)
  (*:setPauseOnLostFocus client pause-on-lost-focus)
  (Mouse:setGrabbed grab-mouse)
  client)

;;; ---------------------------------------------------------------------
;;; client state
;;; ---------------------------------------------------------------------

(define (current-user)
  (let ((client::FabricClient (the-client)))
    client:current-user))

(define (set-current-user! user::FabricUser)
  (let ((client (the-client)))
    (set! client:current-user user)))

(define (current-character)
  (let ((client::FabricClient (the-client)))
    client:current-character))

(define (set-current-character! character::FabricCharacter)
  (let ((client (the-client)))
    (set! client:current-character character)))

(define (current-location)
  (let ((client::FabricClient (the-client)))
    client:current-location))

(define (set-current-location! location::String)
  (let ((client (the-client)))
    (set! client:current-location location)))

;;; ---------------------------------------------------------------------
;;; input handling
;;; ---------------------------------------------------------------------

(define (setup-inputs client::FabricClient)
  (let* ((key-input::KeyInput (*:getKeyInput client))
         (input-manager::InputManager (*:getInputManager client)))
    (*:addMapping input-manager "KeyA" (KeyTrigger key-input:KEY_A))
    (*:addMapping input-manager "KeyD" (KeyTrigger key-input:KEY_D))
    (*:addMapping input-manager "KeyS" (KeyTrigger key-input:KEY_S))
    (*:addMapping input-manager "KeySPACE" (KeyTrigger key-input:KEY_SPACE))
    (*:addMapping input-manager "KeyW" (KeyTrigger key-input:KEY_W))
    (*:addMapping input-manager "KeyX" (KeyTrigger key-input:KEY_X))
    (*:addMapping input-manager "MouseButtonLeft" (MouseButtonTrigger MouseInput:BUTTON_LEFT))
    (*:addMapping input-manager "MouseButtonRight" (MouseButtonTrigger MouseInput:BUTTON_RIGHT))
    (*:addMapping input-manager "MouseDragDown" (MouseAxisTrigger AXIS_Y #t))
    (*:addMapping input-manager "MouseDragUp" (MouseAxisTrigger AXIS_Y #f))
    (*:addMapping input-manager "MouseDragLeft" (MouseAxisTrigger AXIS_X #t))
    (*:addMapping input-manager "MouseDragRight" (MouseAxisTrigger AXIS_X #f))
    (*:addListener input-manager client
                   ;; motion controls
                   "KeyA" "KeyD" "KeyW" "KeyS" "KeySPACE" "KeyX"
                   "MouseButtonLeft" "MouseButtonRight"
                   "MouseDragRight" "MouseDragLeft" "MouseDragUp" "MouseDragDown")))


;;; ---------------------------------------------------------------------
;;; state changes
;;; ---------------------------------------------------------------------

(define (%enqueue-state-change client::FabricClient new-state::FabricClientState)
  (let*((mgr::AppStateManager (*:getStateManager client))
        (old-state::FabricClientState client:state))
    (*:enqueue client
               (runnable (lambda ()
                           (unless (eqv? #!null old-state)
                             (*:detach mgr old-state)
                             (*:cleanup old-state))
                           (*:attach mgr new-state)
                           (*:initialize new-state mgr client))))))

(define (activate-transition-state)
  (let ((client::FabricClient (the-client))
        (transition-state::TransitionState (TransitionState)))
    (%enqueue-state-change client transition-state)))

(define (%activate-supplied-state client::FabricClient new-state::FabricClientState)
  (activate-transition-state)
  (Thread:sleep 500)
  (%enqueue-state-change client new-state))

(define (activate-login-state)
  (%activate-supplied-state (the-client) (LoginState)))

(define (activate-create-character-state)
  (let ((new-state::CreateCharacterState (CreateCharacterState)))
    (%activate-supplied-state (the-client) new-state)))

(define (activate-pick-character-state)
  (let ((new-state::PickCharacterState (PickCharacterState)))
    (%activate-supplied-state (the-client) new-state)))

(define (activate-pick-location-state)
  (let ((new-state::PickLocationState (PickLocationState)))
    (%activate-supplied-state (the-client) new-state)))

(define (activate-play-state)
  (let ((new-state::PlayState (PlayState)))
    (%activate-supplied-state (the-client) new-state)))

;;; ---------------------------------------------------------------------
;;; message handling
;;; ---------------------------------------------------------------------

(define (client-handle-message client::FabricClient source::Client message::Message)
  (cond
   ((ActivateLoginMessage? message)(activate-login-state))
   ((ActivatePickCharacterMessage? message)
    (let ((the-message::ActivatePickCharacterMessage message))
      (activate-pick-character-state)))
   ((ActivateCreateCharacterMessage? message)
    (let ((the-message::ActivateCreateCharacterMessage message))
      (activate-create-character-state)))
   ((ActivatePickLocationMessage? message)
    (let ((the-message::ActivatePickLocationMessage message))
      (set-current-user! the-message:user)
      (set-current-character! the-message:character)
      (activate-pick-location-state)))
   ((ActivatePlayMessage? message)
    (let ((the-message::ActivatePlayMessage message))
      (set-current-user! the-message:user)
      (set-current-character! the-message:character)
      (set-current-location! the-message:location)
      (activate-play-state)))
   (else (client-warn (format #f "~%received an unrecognized message: ~S from source: ~S"
                              message source)))))


;;; ---------------------------------------------------------------------
;;; interactive tools
;;; ---------------------------------------------------------------------

(define (show thing)
  (let ((serializer (get-serializer thing)))
    (if serializer
        (format #t "~A" (serializer thing))
        (format #t "~S" thing))))

(define (init-defaults location)
  (let* ((chars (list (make-random-character)
                      (make-random-character)
                      (make-random-character)))
         (nm "fabric")
         (pw "xhijy9DwOe/QM77n+KK1nDVZyPA=")
         (salt (byte[] -2 -41 58 24 120 42 -67 59))
         (user (make-fabric-user username: nm password-hash: pw password-salt: salt characters: chars)))
    (set-current-user! user)
    (set-current-character! (car chars))
    (set-current-location! location)))

(define (start-client)
  (the-client (make-client))
  (*:start (as FabricClient (the-client))))

(define (stop-client)
  (*:stop (as FabricClient (the-client))))
