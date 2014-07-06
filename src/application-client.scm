;;;; ***********************************************************************
;;;; Name:          application-client.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the game client
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export FabricClient make-client)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require 'list-lib)
(require "utilities-math.scm")
(require "utilities-java.scm")
(require "setting-hubs.scm")
(require "setting-sky.scm")
(require "setting-celestial-objects.scm")
(require "interface-frame.scm")
(require "assets-general.scm")
(require "application-common.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(define-private-alias AppSettings com.jme3.system.AppSettings)
(define-private-alias AssetManager com.jme3.asset.AssetManager)
(define-private-alias BloomFilter com.jme3.post.filters.BloomFilter)
(define-private-alias CameraControl com.jme3.scene.control.CameraControl)
(define-private-alias CameraNode com.jme3.scene.CameraNode)
(define-private-alias FilterPostProcessor com.jme3.post.FilterPostProcessor)
(define-private-alias FlyByCamera com.jme3.input.FlyByCamera)
(define-private-alias HashPMap org.pcollections.HashPMap)
(define-private-alias Mouse org.lwjgl.input.Mouse)
(define-private-alias Node com.jme3.scene.Node)
(define-private-alias Vector2f com.jme3.math.Vector2f)
(define-private-alias Vector3f com.jme3.math.Vector3f)
(define-private-alias ViewPort com.jme3.renderer.ViewPort)

;;; ---------------------------------------------------------------------
;;; FabricClient - the application class
;;; ---------------------------------------------------------------------

(define (%set-client-hub! app::FabricApp key hub)
  (let* ((root::Node (get-key app root-node:))
         (already (get-key app hub:))
         (old-slots::HashPMap (*:getSlots app)))
    (unless (absent? already)
      (*:detachChild root already))
    (*:setSlots app (*:plus old-slots hub: hub))
    (*:attachChild root hub)))

(define-simple-class FabricClient (FabricApp)
  (init: (set-slot-setter! (this) hub: %set-client-hub!)))

;;; ---------------------------------------------------------------------
;;; initialize the client
;;; ---------------------------------------------------------------------

(define (init-scene app)
  (Mouse:setGrabbed #f)
  (set-key! app skybox: (make-client-sky))
  (set-key! app hub: (make-celestial-object (get-key app hub-name:))))

(define (init-lighting app)
  (let* ((asset-manager::AssetManager (get-asset-manager))
         (bloom (BloomFilter BloomFilter:GlowMode:Objects))
         (filter-processor::FilterPostProcessor (FilterPostProcessor asset-manager))
         (viewport::ViewPort (get-key app viewport:)))
    (*:setDownSamplingFactor bloom 2.0)
    (*:setBloomIntensity bloom 2.0)
    (*:addFilter filter-processor bloom)
    (*:addProcessor viewport filter-processor)))

(define (init-player app)
  (set-key! app player: (make-player)))

(define (init-camera app player)
  (let* ((camera::com.jme3.renderer.Camera (get-key app camera:))
         (cam-node::CameraNode (CameraNode "camera" camera))
         (fly-cam::FlyByCamera (get-key app flyby-camera:))
         (player-node::Node (get-key player node:)))
    (*:setEnabled fly-cam #f)
    (*:setControlDir cam-node CameraControl:ControlDirection:SpatialToCamera)
    (*:setFrustumFar camera 30000)
    ;; position the camera behind and above the player and look at the player
    (*:setLocalTranslation cam-node (Vector3f 0 30 -30))
    (*:lookAt cam-node (*:getLocalTranslation player-node) Vector3f:UNIT_Y)
    ;; attach the camera to the player character
    (*:attachChild player-node cam-node)
    cam-node))

(define (init-client app)
  (init-scene app)
  (init-lighting app)
  (init-player app)
  (init-camera app (get-key app player:)))

;;; ---------------------------------------------------------------------
;;; create the client
;;; ---------------------------------------------------------------------

(define (make-client #!optional (hub-name "Earth"))
  (let* ((client (FabricClient))
         (settings::AppSettings (get-key client settings:)))
    ;; set client getters and setters
    
    ;; set client slots
    (set-key! client hub-name: hub-name)
    (set-key! client application-init: init-client)
    ;; set JME3 settings
    (*:setResolution settings 1920 1200)
    (*:setTitle settings "The Fabric")
    (*:setSettingsDialogImage settings "Interface/icon.jpg")
    (*:setSettings client settings)
    (*:setDisplayFps client #f) ; #t to show FPS
    (*:setShowSettings client #t) ; #t to show settings dialog
    (*:setDisplayStatView client #f) ; #t to show stats
    (*:setPauseOnLostFocus client #f)
    client))
