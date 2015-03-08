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

(module-export FabricApp normalize-camera!
               move-node-forward! move-node-backward!
               move-node-left! move-node-right!
               rotate-node-left! rotate-node-right!
               rotate-node-up! rotate-node-down!)

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
(import-as Node com.jme3.scene.Node)
(import-as SimpleApplication com.jme3.app.SimpleApplication)
(import-as Vector3f com.jme3.math.Vector3f)
(import-as VideoRecorderAppState com.jme3.app.state.VideoRecorderAppState)

;;; ---------------------------------------------------------------------
;;; FabricApp - the abstract client application class
;;; ---------------------------------------------------------------------

(defclass FabricApp (SimpleApplication AnalogListener ActionListener)
  (slots:
   (app-settings init-form: (AppSettings #t) getter: getAppSettings)
   (direction type: Vector3f init-form: (Vector3f) getter: getDirection setter: setDirection)
   (speed type: float init-form: 0.0 getter: getSpeed setter: setSpeed)
   (network-client ::com.jme3.network.Client init-form: #!null getter: getNetworkClient setter: setNetworkClient)
   (center-name ::String init-form: #!null getter: getCenterName setter: setCenterName)
   (left-button? init-form: #f getter: getLeftButton setter: setLeftButton)
   (right-button? init-form: #f getter: getRightButton setter: setRightButton)
   (chat-hud init-form: #!null getter: getChatHud setter: setChatHud)
   (video-capture? init-form: #f getter: isVideoCapture setter: setVideoCapture))
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
   ((simpleInitApp) #!abstract))
  (init: (when (*:isVideoCapture (this))
           (*:attach (*:getStateManager (this))
                     (VideoRecorderAppState)))))

;;; player-camera movement
;;; ---------------------------------------------------------------------

(define (normalize-camera! app :: FabricApp)
  (let ((dir :: Vector3f (*:getCameraDirection app)))
    (*:normalizeLocal dir)))

(define (move-node!  app :: FabricApp node :: Node amount :: float invert?)
  (let ((dir :: Vector3f (*:getDirection app))
        (sign (if invert? -1 1)))
    (*:multLocal dir (* sign amount))
    (*:move node dir)))

(define (move-node-forward! app :: FabricApp node :: Node amount :: float)
  (normalize-camera! app)
  (*:setDirection app (*:getCameraDirection app))
  (move-node! app node amount #f))

(define (move-node-backward! app :: FabricApp node :: Node amount :: float)
  (normalize-camera! app)
  (*:setDirection app (*:getCameraDirection app))
  (move-node! app node amount #t))

(define (move-node-left! app :: FabricApp node :: Node amount :: float)
  (*:setDirection app (*:normalizeLocal (*:getLeft (*:getCamera app))))
  (move-node! app node amount #f))

(define (move-node-right! app :: FabricApp node :: Node amount :: float)
  (*:setDirection app (*:normalizeLocal (*:getLeft (*:getCamera app))))
  (move-node! app node amount #t))

(define (rotate-node-right! node :: Node amount :: float)
  (*:rotate node 0 (* -1 amount) 0))

(define (rotate-node-left! node :: Node amount :: float)
  (*:rotate node 0 amount 0))

(define (rotate-node-up! node :: Node amount :: float)
  (*:rotate node (* -1 amount) 0 0))

(define (rotate-node-down! node :: Node amount :: float)
  (*:rotate node amount 0 0))



