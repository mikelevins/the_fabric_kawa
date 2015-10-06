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
 alert
 client-handle-message
 client-warn
 compute-client-warning-rect
 compute-warning-button-rect
 compute-warning-label-rect
 make-client
 setup-inputs
 start-client
 stop-client
 the-client)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require data-assets)
(require data-config)
(require data-users)
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
(require view-alert)
(require view-warning-button)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (only (com jme3 input MouseInput) AXIS_X AXIS_Y))

(import (class com.jme3.app SimpleApplication))
(import (class com.jme3.app.state AbstractAppState AppStateManager))
(import (class com.jme3.asset AssetManager))
(import (class com.jme3.font BitmapFont))
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
(import (class java.lang Thread))
(import (class org.lwjgl.input Mouse))
(import (class tonegod.gui.controls.windows Panel))
(import (class tonegod.gui.controls.text Label))
(import (class tonegod.gui.core Screen))

;;; ---------------------------------------------------------------------
;;; FabricClient
;;; ---------------------------------------------------------------------

(define the-client (make-parameter #!null))

(define-simple-class FabricClient (SimpleApplication AnalogListener ActionListener)
  ;; slots
  (user::FabricUser init: #!null)
  (username::String init: #!null)
  (password-hash::String init: #!null)
  (password-salt::byte[] init: #!null)
  (character::FabricCharacter init: #!null)
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
         (activate-transition-state app)
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

(define (start-client)
  (the-client (make-client))
  (let* ((user::FabricUser (get-user "fabric"))
         (characters user:characters)
         (character::FabricCharacter (car characters))
         (client::FabricClient (the-client)))
    (set! client:character character)
    (*:start (as FabricClient (the-client)))))

(define (stop-client)
  (*:stop (as FabricClient (the-client))))

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
;;; a warning dialog
;;; ---------------------------------------------------------------------

(define (compute-client-warning-rect screen::Screen)
  (let* ((screen-width (*:getWidth screen))
         (screen-height (*:getHeight screen))
         (width 512)
         (height 256)
         (left (- (/ screen-width 2)
                  (/ width 2)))
         (top (- (/ screen-height 2)
                 (/ height 2))))
    (make-rectangle left top width height)))

(define (compute-warning-button-rect rect)
  (let* ((width 128)
         (height 40)
         (left (- (/ (get-width rect) 2)
                  (/ width 2)))
         (top (- (get-height rect)
                 (+ height 16))))
    (make-rectangle left top width height)))

(define (compute-warning-label-rect rect)
  (let* ((left 16)
         (top 16)
         (width (- (get-width rect)
                   (* 2 left)))
         (height  (- (get-height rect)
                     (* 2 top))))
    (make-rectangle left top width height)))

(define (client-warn message::String)
  (let* ((client::FabricClient (the-client))
         (screen::Screen client:screen)
         (align BitmapFont:Align)
         (panel-rect (compute-client-warning-rect screen))
         (panel (Panel screen "WarningDialog"
                       (Vector2f (get-left panel-rect)(get-top panel-rect))
                       (Vector2f (get-width panel-rect)(get-height panel-rect))))
         (button-rect (compute-warning-button-rect panel-rect))
         (button (WarningDialogButton client screen panel "Okay"
                                      (Vector2f (get-left button-rect)(get-top button-rect))
                                      (Vector2f (get-width button-rect)(get-height button-rect))))
         (label-rect (compute-warning-label-rect panel-rect))
         (label (Label screen "WarningLabel"
                       (Vector2f (get-left label-rect)(get-top label-rect))
                       (Vector2f (get-width label-rect)(get-height label-rect)))))
    (*:setText label message)
    (*:setTextAlign label align:Center)
    (*:addChild panel label)
    (*:addChild panel button)
    (*:enqueue client (runnable (lambda ()(*:addElement screen panel))))))

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

(define (activate-transition-state client::FabricClient)
  (let ((transition-state::TransitionState (TransitionState)))
    (%enqueue-state-change client transition-state)))

(define (%activate-supplied-state client::FabricClient new-state::FabricClientState)
  (activate-transition-state client)
  (Thread:sleep 500)
  (%enqueue-state-change client new-state))

(define (activate-login-state client::FabricClient)
  (%activate-supplied-state client (LoginState)))

(define (activate-create-character-state client::FabricClient user::FabricUser)
  (let ((new-state::CreateCharacterState (CreateCharacterState user)))
    (%activate-supplied-state client new-state)))

(define (activate-pick-character-state client::FabricClient user::FabricUser)
  (let ((new-state::PickCharacterState (PickCharacterState user)))
    (%activate-supplied-state client new-state)))

(define (activate-pick-location-state client::FabricClient user::FabricUser character::FabricCharacter)
  (let ((new-state::PickLocationState (PickLocationState user character)))
    (%activate-supplied-state client new-state)))

(define (activate-play-state client::FabricClient user::FabricUser
                             character::FabricCharacter location::String)
  (let ((new-state::PlayState (PlayState user character location)))
    (%activate-supplied-state client new-state)))

;;; ---------------------------------------------------------------------
;;; message handling
;;; ---------------------------------------------------------------------

(define (client-handle-message client::FabricClient source::Client message::Message)
  (cond
   ((ActivateLoginMessage? message)(activate-login-state client))
   ((ActivatePickCharacterMessage? message)
    (let ((the-message::ActivatePickCharacterMessage message))
      (activate-pick-character-state client the-message:user)))
   ((ActivateCreateCharacterMessage? message)
    (let ((the-message::ActivateCreateCharacterMessage message))
      (activate-create-character-state client the-message:user)))
   ((ActivatePickLocationMessage? message)
    (let ((the-message::ActivatePickLocationMessage message))
      (activate-pick-location-state client the-message:user the-message:character)))
   ((ActivatePlayMessage? message)
    (let ((the-message::ActivatePlayMessage message))
      (activate-play-state client the-message:user the-message:character the-message:location)))
   (else (client-warn (format #f "~%received an unrecognized message: ~S from source: ~S"
                              message source)))))
