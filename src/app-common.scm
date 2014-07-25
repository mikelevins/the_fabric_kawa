;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          app-common.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       definitions shared by client and workshop
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export FabricApp camera-left normalize-camera!)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "syntax-classes.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as ActionListener com.jme3.input.controls.ActionListener)
(import-as AnalogListener com.jme3.input.controls.AnalogListener)
(import-as AppSettings com.jme3.system.AppSettings)
(import-as SimpleApplication com.jme3.app.SimpleApplication)
(import-as Vector3f com.jme3.math.Vector3f)

;;; ---------------------------------------------------------------------
;;; FabricApp - the abstract client application class
;;; ---------------------------------------------------------------------

(defclass FabricApp (SimpleApplication AnalogListener ActionListener)
  (slots:
   (app-settings init-form: (AppSettings #t) getter: getAppSettings)
   (direction type: Vector3f init-form: (Vector3f) getter: getDirection setter: setDirection)
   (network-client ::com.jme3.network.Client init-form: #!null getter: getNetworkClient setter: setNetworkClient)
   (center-name ::String init-form: #!null getter: getCenterName setter: setCenterName)
   (left-button? init-form: #f getter: getLeftButton setter: setLeftButton)
   (right-button? init-form: #f getter: getRightButton setter: setRightButton)
   (chat-hud init-form: #!null getter: getChatHud setter: setChatHud))
  (methods:
   ((getCameraDirection) (*:getDirection cam))
   ((getAudioRenderer) audioRenderer)
   ((getViewport) viewPort)
   ((getInputManager) inputManager)
   ((getStateManager) stateManager)
   ((getGuiNode) guiNode)
   ((getGuiFont) guiFont)
   ((getKeyInput) keyInput)
   ((onAnalog name value tpf) #!abstract)
   ((onAction name key-pressed? tpf) #!abstract)
   ((simpleInitApp) #!abstract)))


;;; camera control
;;; ---------------------------------------------------------------------

(define (camera-left app :: FabricApp)
  (*:getLeft (*:getCamera app)))

(define (normalize-camera! app :: FabricApp)
  (*:normalizeLocal (*:getCameraDirection app)))

