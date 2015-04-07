;;;; ***********************************************************************
;;;;
;;;; Name:          client-main.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       fabric client main program
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************
(format #t "~%loading client-main.cm")

(module-export
 client-set-create-character-state!
 client-set-login-state!
 client-set-pick-character-state!
 client-set-play-state!
 client-set-transit-state!
 FabricClient
 make-client)

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
(require "util-lists.scm")
(require "data-nodes.scm")
(require "syntax-classes.scm")
(require "client-state.scm")
(require "state-login.scm")
(require "state-create-character.scm")
(require "state-pick-character.scm")
(require "state-play.scm")
(require "state-transit.scm")

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
   (app-settings init-form: #!null getter: getAppSettings)
   (client-state init-form: #!null getter: getClientState setter: setClientState)
   (user init-form: #!null getter: getUser setter: setUser)
   (character init-form: #!null getter: getCharacter setter: setCharacter)
   (screen init-form: #!null getter: getScreen setter: setScreen)
   (fabric-node init-form: #f getter: getFabricNode setter: setFabricNode)) 
  (methods:
   ((getCamera) cam)
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
   ((simpleInitApp)(begin (*:setEnabled (*:getFlyByCamera (this)) #f)
                          #!void))))

;;; ---------------------------------------------------------------------
;;; construct the client app
;;; ---------------------------------------------------------------------

;;; (make-client #!key )
;;; ---------------------------------------------------------------------
;;; returns a newly-created and -configured instance of the
;;; client application. The game can be started by calling
;;; the new client application's start method.

(define (make-client #!key
                     (client (FabricClient))
                     (settings (AppSettings #t))
                     (screen-width 1920)
                     (screen-height 1200)
                     (title "The Fabric")
                     (settings-image "Interface/icon.jpg")
                     (show-fps #f)
                     (show-settings #t)
                     (show-statistics #f)
                     (pause-on-lost-focus #f)
                     (grab-mouse #f))
  (*:setResolution (as AppSettings settings) screen-width screen-height)
  (*:setTitle (as AppSettings settings) title)
  (*:setSettingsDialogImage (as AppSettings settings) settings-image)
  (*:setSettings (as FabricClient client) (as AppSettings settings))
  (*:setDisplayFps (as FabricClient client) show-fps)
  (*:setShowSettings (as FabricClient client) show-settings)
  (*:setDisplayStatView (as FabricClient client) show-statistics)
  (*:setPauseOnLostFocus (as FabricClient client) pause-on-lost-focus)
  (Mouse:setGrabbed grab-mouse)
  client)

;;; IMPORTANT
;;; ---------------------------------------------------------------------
;;; these functions are private and are not thread-safe; do not call
;;; them directly; rely on enqueue-client-state-update
;;; ---------------------------------------------------------------------
;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(define (%detach-and-cleanup-current-state! client::FabricClient)
  (let ((current-state (*:getClientState client))
        (mgr (*:getStateManager client)))
    (unless (jnull? current-state)
      (*:detach mgr current-state)
      (*:setClientState client #!null))))

(define (%attach-and-activate-new-state! client::FabricClient new-state)
  (let ((mgr (*:getStateManager client)))
    (*:setClientState client new-state)
    (*:attach mgr new-state)))

(define (%update-client-state! client::FabricClient new-state)
  (unless (equal? new-state (*:getClientState client))
    (%detach-and-cleanup-current-state! client)
    (%attach-and-activate-new-state! client new-state)))

;;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; set the client's main state
;;; ---------------------------------------------------------------------

(define (%enqueue-state-change client::FabricClient new-state)
  (*:enqueue client
             (runnable (lambda ()
                         (%update-client-state! client new-state)))))

(define (client-set-login-state! client::FabricClient)
  (%enqueue-state-change client (make-login-state client)))

(define (client-set-create-character-state! client::FabricClient)
  (%enqueue-state-change client (make-create-character-state client)))

(define (client-set-pick-character-state! client::FabricClient)
  (%enqueue-state-change client (make-pick-character-state client)))

(define (client-set-play-state! client::FabricClient #!optional (node-name "The Sun"))
  (%enqueue-state-change client (make-play-state client node-name)))

(define (client-set-transit-state! client::FabricClient #!key (from "The Sun")(to "Earth"))
  (%enqueue-state-change client (make-transit-state client from: from to: to)))

