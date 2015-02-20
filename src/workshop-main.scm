;;;; ***********************************************************************
;;;;
;;;; Name:          workshop-main.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       fabric client main program
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export make-workshop Workshop)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; 

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-error.scm")
(require "util-java.scm")
(require "syntax-classes.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as ActionListener com.jme3.input.controls.ActionListener)
(import-as AnalogListener com.jme3.input.controls.AnalogListener)
(import-as AppSettings com.jme3.system.AppSettings)
(import-as AppStateManager com.jme3.app.state.AppStateManager)
(import-as BitmapFont com.jme3.font.BitmapFont)
(import-as EffectEvent tonegod.gui.effects.Effect:EffectEvent)
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
;;; Workshop - the workshop application class
;;; ---------------------------------------------------------------------

;;; 
;;; ---------------------------------------------------------------------
;;; 

(defclass Workshop (SimpleApplication AnalogListener ActionListener)
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
   ((simpleInitApp)(init-workshop (this)))))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (init-workshop app::Workshop)
  ;; don't seize the mouse from the player
  (Mouse:setGrabbed #f)
  ;; disable the fly-by camera
  (*:setEnabled (*:getFlyByCamera app) #f)
  ;; return void to make Java happy
  #!void)

;;; ---------------------------------------------------------------------
;;; construct the workshop app
;;; ---------------------------------------------------------------------

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (make-workshop #!optional (center #f))
  (let* ((app::Workshop (Workshop))
	 (settings::AppSettings (*:getAppSettings app)))
    (*:setResolution settings 1920 1200)
    (*:setTitle settings "Fabric Workshop")
    (*:setSettingsDialogImage settings "Interface/icon.jpg")
    (*:setSettings app settings)
    (*:setDisplayFps app #t) ; #t to show FPS
    (*:setShowSettings app #t) ; #t to show settings dialog
    (*:setDisplayStatView app #t) ; #t to show stats
    (*:setPauseOnLostFocus app #t)
    app))

;;; (define $shop (make-workshop))
;;; (*:start $shop)


