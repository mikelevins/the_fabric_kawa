;;;; ***********************************************************************
;;;;
;;;; Name:          client-brocade.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the fabric client presentation server
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export make-brocade)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; brocade is a presentation server that uses the 9P protocol to
;;; present its services to client programs, whether in-process or
;;; running in separate processes.

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-error.scm")
(require "util-java.scm")
(require "syntax-classes.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as AppSettings com.jme3.system.AppSettings)
(import-as Mouse org.lwjgl.input.Mouse)
(import-as ProcessFile j9p.ns.handlers.ProcessFile)
(import-as SimpleApplication com.jme3.app.SimpleApplication)

;;; ---------------------------------------------------------------------
;;; brocade
;;; ---------------------------------------------------------------------

(defclass BrocadeServer (ProcessFile:Listener)
  (slots:
   (app init-form: #!null getter: getApp setter: setApp))
  (methods:
   ((asInput bytes::byte[] how-many::long) #f)))

(defclass BrocadeApp (SimpleApplication)
  (slots:
   (app-settings init-form: (AppSettings #t) getter: getAppSettings))
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
   ((simpleInitApp)(init-brocade (this)))))

;;; ---------------------------------------------------------------------
;;; initialization
;;; ---------------------------------------------------------------------

(define (init-brocade app::BrocadeApp)
  ;; don't seize the mouse from the player
  (Mouse:setGrabbed #f)
  ;; disable the fly-by camera
  (*:setEnabled (*:getFlyByCamera app) #f)
  ;; return void to make Java happy
  #!void)

;;; ---------------------------------------------------------------------
;;; construct the brocade server
;;; ---------------------------------------------------------------------

(define (make-brocade #!optional (center #f))
  (let* ((app :: BrocadeApp (BrocadeApp))
	 (settings :: AppSettings (*:getAppSettings app)))
    ;;(Serializer:registerClass ChatMessage)
    (*:setResolution settings 1920 1200)
    (*:setTitle settings "The Fabric")
    (*:setSettingsDialogImage settings "Interface/icon.jpg")
    (*:setSettings app settings)
    (*:setDisplayFps app #f) ; #t to show FPS
    (*:setShowSettings app #t) ; #t to show settings dialog
    (*:setDisplayStatView app #f) ; #t to show stats
    (*:setPauseOnLostFocus app #f)
    app))

;;; (define $client::BrocadeApp (make-brocade))
;;; (*:start $client)
