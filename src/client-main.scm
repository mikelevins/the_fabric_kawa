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
 set-client-state!)

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
(require "model-statepool.scm")
(require "gamestates.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as ActionListener com.jme3.input.controls.ActionListener)
(import-as AnalogListener com.jme3.input.controls.AnalogListener)
(import-as AppSettings com.jme3.system.AppSettings)
(import-as Mouse org.lwjgl.input.Mouse)
(import-as Screen tonegod.gui.core.Screen)
(import-as SimpleApplication com.jme3.app.SimpleApplication)

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
   (game-state init-form: #!null getter: getGameState setter: setGameState)
   (screen init-form: #!null))
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
;;; change client states
;;; ---------------------------------------------------------------------

(define (%client-state-different? client::FabricClient new-state)
  (let ((current-state (*:getGameState client)))
    (or (and (jnull? current-state)
             (not (jnull? new-state)))
        (and (not (jnull? current-state))
             (jnull? new-state))
        (not (eq? current-state new-state)))))

;;; IMPORTANT
;;; ---------------------------------------------------------------------
;;; these functions are private and are not thread-safe; do not call
;;; them directly; rely on enqueue-client-state-update
;;; ---------------------------------------------------------------------
;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define (%detach-and-cleanup-current-state client::FabricClient)
  (let ((current-state (*:getGameState client))
        (mgr (*:getStateManager client)))
    (unless (jnull? current-state)
      (*:detach mgr current-state)
      (*:setGameState client #!null)
      (*:cleanupDetached (as FabricGameState current-state) mgr client))))

(define (%attach-and-activate-new-state client::FabricClient new-state)
  (let ((mgr (*:getStateManager client)))
    (*:prepareToAttach (as FabricGameState new-state) mgr client)
    (*:setGameState client new-state)
    (*:attach mgr new-state)))

(define (%update-client-state client::FabricClient new-state)
  (when (%client-state-different? client new-state)
    (%detach-and-cleanup-current-state client)
    (%attach-and-activate-new-state client new-state)))

;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;; ---------------------------------------------------------------------

(define (enqueue-client-state-update client::FabricClient new-state)
  (*:enqueue client
             (runnable (lambda ()
                         (%update-client-state client new-state)))))

;;; ---------------------------------------------------------------------
;;; construct the client app
;;; ---------------------------------------------------------------------

;;; (make-client #!optional (start-mode 'create-character))
;;; ---------------------------------------------------------------------
;;; returns a newly-created and -configured instance of the
;;; client application. The game can be started by calling
;;; the new client application's start method.

(define (make-client)
  (let* ((client::FabricClient (FabricClient))
	 (settings::AppSettings (*:getAppSettings client)))
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

(define (set-client-state! client::FabricClient state-name)
  (let ((new-state (get-appstate client state-name)))
    (when new-state
      (enqueue-client-state-update client new-state))))

