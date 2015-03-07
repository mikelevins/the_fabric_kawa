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
;;; the client application class
;;; ---------------------------------------------------------------------

;;; CLASS FabricClient
;;; ---------------------------------------------------------------------
;;; a SimpleApplication subclass that represents the Fabric client
;;; application. FabricClient renders the game world, provides all
;;; user interface, and manages network connections that make it
;;; possible to play the game.

(defclass FabricClient (SimpleApplication AnalogListener ActionListener)
  (slots:
   (app-settings init-form: (AppSettings #t) getter: getAppSettings)
   (mode-name init-form: #f getter: getModeName setter: setModeName)
   (mode-state init-form: #!null getter: getModeState setter: setModeState)
   (screen init-form: #!null)
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
   ((getScreen)(begin (if (jnull? screen)(set! screen (Screen (this))))
                      screen))
   ;; stubs for now; fix up in AppState
   ((onAnalog name value tpf) #f) 
   ((onAction name key-pressed? tpf) #f)
   ;; init the app
   ((simpleInitApp)(init-client (this)))))

;;; ---------------------------------------------------------------------
;;; client init
;;; ---------------------------------------------------------------------

(define (init-client client::FabricClient)
  ;; disable the fly-by camera
  (*:setEnabled (*:getFlyByCamera client) #f)
  ;; return void to make Java happy
  #!void)

;;; ---------------------------------------------------------------------
;;; construct the client app
;;; ---------------------------------------------------------------------
;;; to change game modes, call enqueue-mode-update
;;; it enqueues a lambda onto the rendering thread that when
;;; called, detaches and cleans up the old game state,
;;; then attaches and initializes the new one

(define (mode-name->app-state mode)
  (case mode
    ((#f) #!null)
    ((login)(LoginAppState))
    ((pick-character)(error "mode-name->app-state: pick-character mode is not yet implemented" ))
    ((create-character)(CharacterCreatorAppState))
    ((play)(error "mode-name->app-state: play mode is not yet implemented" ))
    (else #!null)))

(define (mode-changed? client::FabricClient mode-name)
  (not (equal? mode-name
               (*:getModeName client))))

(define (clear-game-state! client::FabricClient)
  ;; detach the current game state
  (let ((current-state (*:getModeState client))
        (mgr (*:getStateManager client)))
    (*:detach mgr current-state)
    (*:setModeName client #f)
    (*:setModeState client #!null)))

(define (update-game-mode! client::FabricClient mode-name)
  (*:setModeName client mode-name))

(define (update-game-state! client::FabricClient)
  (let* ((mode (*:getModeName client))
         (new-state (mode-name->app-state mode))
         (mgr::AppStateManager (*:getStateManager client)))
    (if (not (jnull? new-state))
        (begin (*:setModeState client #!null)
               (*:attach mgr new-state)))))

(define (set-game-mode! client::FabricClient mode-name)
  (if (mode-changed? client mode-name)
      (begin (clear-game-state! client)
             ;; enqueue the next update so it doesn't happen while the
             ;; cleanup is happening from the previous state being
             ;; detached
             (*:enqueue client
                        (runnable (lambda ()
                                    (update-game-mode! client mode-name)
                                    (update-game-state! client)))))))

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
    ;; don't seize the mouse from the player
    (Mouse:setGrabbed #f)
    client))


