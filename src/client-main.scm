;;;; ***********************************************************************
;;;;
;;;; Name:          client-main.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       fabric client main program
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 FabricClient
 make-client
 enqueue-mode-update)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; This file implements the main Fabric client, the program that
;;; renders the game world and provides the user interface of the
;;; game.

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-error.scm")
(require "util-java.scm")
(require "syntax-classes.scm")
(require "net-messaging.scm")
(require "appstate-login.scm")
(require "appstate-character-creator.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as AbstractAppState com.jme3.app.state.AbstractAppState)
(import-as ActionListener com.jme3.input.controls.ActionListener)
(import-as AnalogListener com.jme3.input.controls.AnalogListener)
(import-as AppSettings com.jme3.system.AppSettings)
(import-as AppStateManager com.jme3.app.state.AppStateManager)
(import-as BitmapFont com.jme3.font.BitmapFont)
(import-as Client com.jme3.network.Client)
(import-as EffectEvent tonegod.gui.effects.Effect:EffectEvent)
(import-as MessageListener com.jme3.network.MessageListener)
(import-as Mouse org.lwjgl.input.Mouse)
(import-as Node com.jme3.scene.Node)
(import-as PI com.jme3.math.FastMath:PI)
(import-as Screen tonegod.gui.core.Screen)
(import-as Serializer com.jme3.network.serializing.Serializer)
(import-as SimpleApplication com.jme3.app.SimpleApplication)
(import-as String java.lang.String)
(import-as TextField tonegod.gui.controls.text.TextField)
(import-as TLabel tonegod.gui.controls.text.Label)
(import-as VideoRecorderAppState com.jme3.app.state.VideoRecorderAppState)

;;; ---------------------------------------------------------------------
;;; FabricClient - the client application class
;;; ---------------------------------------------------------------------

(define +mode-names+
  '(login pick-character create-character play))

(define (mode-name->class mode)
  (case mode
    ((#f) #!null)
    ((login)(error "mode-name->class: login mode is not yet implemented" ))
    ((pick-character)(error "mode-name->class: pick-character mode is not yet implemented" ))
    ((create-character) CharacterCreatorAppState)
    ((play)(error "mode-name->class: play mode is not yet implemented" ))
    (else #!null)))

(define (mode-name->app-state mode)
  (case mode
    ((#f) #!null)
    ((login)(error "mode-name->app-state: login mode is not yet implemented" ))
    ((pick-character)(error "mode-name->app-state: pick-character mode is not yet implemented" ))
    ((create-character)(CharacterCreatorAppState))
    ((play)(error "mode-name->app-state: play mode is not yet implemented" ))
    (else #!null)))

;;; CLASS FabricClient
;;; ---------------------------------------------------------------------
;;; a SimpleApplication subclass that represents the Fabric client
;;; application. FabricClient renders the game world, provides all
;;; user interface, and manages network connections that make it
;;; possible to play the game.

(defclass FabricClient (SimpleApplication AnalogListener ActionListener)
  (slots:
   (app-settings init-form: (AppSettings #t) getter: getAppSettings)
   (mode-name init-form: 'create-character getter: getModeName setter: setModeName)
   (mode-state init-form: #!null getter: getModeState setter: setModeState)
   (network-client::com.jme3.network.Client
    init-form: #!null getter: getNetworkClient setter: setNetworkClient))
  (methods:
   ((getCameraDirection) (*:getDirection cam))
   ((getAudioRenderer) audioRenderer)
   ((getViewport) viewPort)
   ((getInputManager) inputManager)
   ((getStateManager) stateManager)
   ((getGuiNode) guiNode)
   ((getGuiFont) guiFont)
   ((getKeyInput) keyInput)
   ;; stubs for now; fix up in AppState
   ((onAnalog name value tpf) #f) 
   ((onAction name key-pressed? tpf) #f)
   ;; init the app
   ((simpleInitApp)(init-client (this)))))

;;; ---------------------------------------------------------------------
;;; client init
;;; ---------------------------------------------------------------------

;;; (init-client app::FabricClient)
;;; ---------------------------------------------------------------------
;;; initializes the client, includng setting up the initial AppState,
;;; camera, and event handlers

(define (init-client app::FabricClient)
  ;; don't seize the mouse from the player
  (Mouse:setGrabbed #f)
  ;; disable the fly-by camera
  (*:setEnabled (*:getFlyByCamera app) #f)
  ;; return void to make Java happy
  #!void)

;;; ---------------------------------------------------------------------
;;; construct the client app
;;; ---------------------------------------------------------------------

;;; modifies the game mode; must be called within the scene graph's
;;; thread; for interactive updates, use enqueue-mode-update
(define (set-game-mode! client::FabricClient mode-name)
  (let ((new-mode-class (mode-name->class mode-name))
        (current-mode-name (*:getModeName client))
        (current-mode-state (*:getModeState client))
        (state-manager (*:getStateManager client)))
    (if (jnull? new-mode-class)
        ;; null new mode; clear the game state
        (begin (if (not (jnull? current-mode-state))
                   (*:detach state-manager current-mode-state))
               (*:setModeName client #f)
               (*:setModeState client #!null))
        ;; new mode; clear any old state, then
        ;; initialize and attach the new state
        (let ((new-mode-state (mode-name->app-state mode-name)))
          (if (jnull? new-mode-state)
              ;; something went wrong with creating the new state
              (error "Creating the game mode failed" mode-name)
              ;; new mode state created; initialize and attach it
              (begin (if (not (jnull? current-mode-state))
                         (*:detach state-manager current-mode-state))
                     (*:initialize new-mode-state state-manager client)
                     (*:attach state-manager new-mode-state)
                     (*:setModeName client mode-name)
                     (*:setModeState client new-mode-state)))))))

(define (enqueue-mode-update client::FabricClient mode-name)
  (*:enqueue client (runnable (lambda ()(set-game-mode! client mode-name)))))

;;; (make-client #!optional (start-mode 'create-character))
;;; ---------------------------------------------------------------------
;;; returns a newly-created and -configured instance of the
;;; client application. The game can be started by calling
;;; the new client application's start method.

(define (make-client)
  (let* ((client::FabricClient (FabricClient))
	 (settings::AppSettings (*:getAppSettings client)))
    (Serializer:registerClass ChatMessage)
    (Serializer:registerClass RequestLoginMessage)
    (Serializer:registerClass ResponseLoginMessage )
    (Serializer:registerClass RequestCreateAccountMessage)
    (Serializer:registerClass ResponseCreateAccountMessage)
    (*:setResolution settings 1920 1200)
    (*:setTitle settings "The Fabric")
    (*:setSettingsDialogImage settings "Interface/icon.jpg")
    (*:setSettings client settings)
    (*:setDisplayFps client #f) ; #t to show FPS
    (*:setShowSettings client #t) ; #t to show settings dialog
    (*:setDisplayStatView client #f) ; #t to show stats
    (*:setPauseOnLostFocus client #f)
    client))


