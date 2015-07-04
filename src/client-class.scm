;;;; ***********************************************************************
;;;;
;;;; Name:          client-class.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       The FabricClient class
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-error.scm")
(require "util-java.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as AppSettings com.jme3.system.AppSettings)
(import-as Mouse org.lwjgl.input.Mouse)
(import-as Screen tonegod.gui.core.Screen)
(import-as SimpleApplication com.jme3.app.SimpleApplication)

;;; ---------------------------------------------------------------------
;;; FabricClient
;;; ---------------------------------------------------------------------

(define-simple-class FabricClient (SimpleApplication)
  ;; slots
  ;; app-settings
  (app-settings init: #!null)
  ((getAppSettings) app-settings)
  ((setAppSettings settings) (set! app-settings settings))
  ;; client-state
  (client-state init: #!null)
  ((getClientState) client-state)
  ((setClientState state) (set! client-state state))
  ;; user
  (user init: #!null)
  ((getUser) user)
  ((setUser usr) (set! user usr))
  ;; character
  (character init: #!null)
  ((getCharacter) character)
  ((setCharacter char) (set! character char))
  ;; screen
  (screen init: #!null)
  ((getScreen) screen)
  ((setScreen char) (set! screen char))
  ;; other accessors
  ((getCamera) cam)
  ((getCameraDirection) (*:getDirection cam))
  ((getAudioRenderer) audioRenderer)
  ((getViewport) viewPort)
  ((getInputManager) inputManager)
  ((getStateManager) stateManager)
  ((getGuiNode) guiNode)
  ((getGuiFont) guiFont)
  ((getKeyInput) keyInput)
  ((getScreen)(begin (if (eqv? screen #!null)(set! screen (Screen (this))))
                     screen))
  ;; init
  ((simpleInitApp) (init-app (this))))

(define (init-app app::FabricClient)
  (begin (*:setEnabled (*:getFlyByCamera app) #f)
         #!void))

;;; ---------------------------------------------------------------------
;;; construct the client app
;;; ---------------------------------------------------------------------

;;; (make-client #!key client settings screen-width screen-height title
;;;                    settings-image show-fps show-settings
;;;                    show-statistics pause-on-lost-focus grab-mouse)
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


;;; PRIVATE
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
