;;;; ***********************************************************************
;;;; Name:          application-workshop.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the builder app
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export FabricWorkshop make-workshop)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require 'list-lib)
(require "utilities-math.scm")
(require "utilities-java.scm")
(require "setting-lighting.scm")
(require "setting-hubs.scm")
(require "setting-sky.scm")
(require "setting-celestial-objects.scm")
(require "interface-frame.scm")
(require "assets-general.scm")
(require "ui-workshop-inspector.scm")
(require "ui-workshop-palette.scm")
(require "application-common.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(define-private-alias AppSettings com.jme3.system.AppSettings)
(define-private-alias Camera com.jme3.renderer.Camera)
(define-private-alias FlyByCamera com.jme3.input.FlyByCamera)
(define-private-alias HashPMap org.pcollections.HashPMap)
(define-private-alias Mouse org.lwjgl.input.Mouse)
(define-private-alias Node com.jme3.scene.Node)
(define-private-alias Screen tonegod.gui.core.Screen)
(define-private-alias Vector3f com.jme3.math.Vector3f)

;;; ---------------------------------------------------------------------
;;; FabricWorkshop - the application class
;;; ---------------------------------------------------------------------

(define (%set-focus-object! app::FabricApp key val)
  (let* ((focus-object (make-celestial-object val))
         (root::Node (get-key app root-node:))
         (already (get-key app focus-object:))
         (old-slots::HashPMap (*:getSlots app)))
    (unless (absent? already)
      (*:detachChild root already))
    (*:setSlots app (*:plus old-slots focus-object: focus-object))
    (*:attachChild root focus-object)
    (*:rotate focus-object (degrees->radians -90) 0 0)))

(define-simple-class FabricWorkshop (FabricApp)
  (init: (set-slot-setter! (this) focus-object: %set-focus-object!)))

;;; ---------------------------------------------------------------------
;;; set up the UI
;;; ---------------------------------------------------------------------

(define (setup-workshop-gui app)
  (let* ((screen::Screen (get-key app gui-screen:))
         (inspector (make-inspector app screen))
         (palette (make-palette app screen)))
    (*:addElement screen inspector)
    (*:addElement screen palette)
    app))

;;; ---------------------------------------------------------------------
;;; initialize the workshop
;;; ---------------------------------------------------------------------

(define (init-workshop app)
  (let ((default-sky (make-workshop-sky))
        (cam::Camera (get-key app camera:))
        (fly-cam::FlyByCamera (get-key app flyby-camera:)))
    (*:setFrustumFar cam 30000)
    (*:setLocation cam (Vector3f 0 0 8000))
    (init-lighting app)
    (setup-workshop-gui app)
    (Mouse:setGrabbed #f)
    (*:setDragToRotate fly-cam #t)
    (*:setMoveSpeed fly-cam 800)
    (set-key! app skybox: default-sky)
    (set-key! app focus-object: "Earth")))

;;; ---------------------------------------------------------------------
;;; create the workshop
;;; ---------------------------------------------------------------------

(define (make-workshop)
  (let* ((shop (FabricWorkshop))
         (settings::AppSettings (get-key shop settings:)))
    (set-key! shop application-init: init-workshop)
    (*:setResolution settings 1920 1200)
    (*:setTitle settings "The Fabric")
    (*:setSettingsDialogImage settings "Interface/icon.jpg")
    (*:setSettings shop settings)
    (*:setDisplayFps shop #t) ; #t to show FPS
    (*:setShowSettings shop #t) ; #t to show settings dialog
    (*:setDisplayStatView shop #t) ; #t to show stats
    (*:setPauseOnLostFocus shop #f)
    shop))
