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
(require "controls-client-inputs.scm")
(require "application-common.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(define-private-alias ActionListener com.jme3.input.controls.ActionListener)
(define-private-alias AnalogListener com.jme3.input.controls.AnalogListener)
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
(define-private-alias PI com.jme3.math.FastMath:PI)
(define-private-alias Quaternion com.jme3.math.Quaternion)
(define-private-alias Vector2f com.jme3.math.Vector2f)
(define-private-alias Vector3f com.jme3.math.Vector3f)
(define-private-alias ViewPort com.jme3.renderer.ViewPort)

;;; ---------------------------------------------------------------------
;;; FabricClient - the application class
;;; ---------------------------------------------------------------------

;;; slot accessors

(define (%set-client-hub! app::FabricApp key hub)
  (let* ((root::Node (get-key app root-node:))
         (already (get-key app hub:))
         (old-slots::HashPMap (*:getSlots app)))
    (unless (absent? already)
      (*:detachChild root already))
    (*:setSlots app (*:plus old-slots hub: hub))
    (*:attachChild root hub)))

(define (%set-client-inputs! app::FabricApp key input-specs)
  (let ((mgr (get-key app input-manager:)))
    (for-each (lambda (spec)
                (let ((name (input-spec-name spec))
                      (signal (input-spec-signal spec)))
                  (*:addMapping mgr name signal)
                  (*:addListener mgr app name)))
              input-specs)))

;;; the class

(define-simple-class FabricClient (FabricApp AnalogListener ActionListener)
  ;; slots
  (direction ::Vector3f init-form: (Vector3f))
  ((getDirection) direction)
  ((setDirection dir) (set! direction dir))

  (left-button? init-form: #f)
  ((getLeftButton) left-button?)
  ((setLeftButton down?) (set! left-button? down?))

  (right-button? init-form: #f)
  ((getRightButton) right-button?)
  ((setRightButton down?) (set! right-button? down?))

  ;; AnalogListener and ActionListener implementation
  ((onAnalog name value tpf)(handle-client-analog-event (this) name value tpf))
  ((onAction name key-pressed? tpf)(handle-action-event (this) name key-pressed? tpf))
  ;; local initialization
  (init: (begin (set-slot-getter! (this) direction: (lambda (app key)(*:getDirection app)))
                (set-slot-getter! (this) key-input: (lambda (app key)(*:getKeyInput app)))
                (set-slot-getter! (this) input-manager: (lambda (app key)(*:getInputManager app)))
                (set-slot-getter! (this) left-button: (lambda (app key)(*:getLeftButton app)))
                (set-slot-getter! (this) right-button: (lambda (app key)(*:getRightButton app)))
                
                (set-slot-setter! (this) direction: (lambda (app key val)(*:setDirection app val)))
                (set-slot-setter! (this) hub: %set-client-hub!)
                (set-slot-setter! (this) inputs: %set-client-inputs!)
                (set-slot-setter! (this) left-button: (lambda (app key val)(*:setLeftButton app val)))
                (set-slot-setter! (this) right-button: (lambda (app key val)(*:setRightButton app val))))))

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
  (let* ((root::Node (get-key app root-node:))
         (player (make-player))
         (player-node::Node (get-key player node:)))
    (set-key! app player: player)
    (*:setLocalTranslation player-node 0.0 8000.0 0.0)
    (let ((rotation (Quaternion))
          (pitch-axis (Vector3f 1 0 0)))
      ;; PI radians points us right at the center
      (*:fromAngleAxis rotation PI pitch-axis)
      (*:setLocalRotation player-node rotation)
      (*:attachChild root player-node))))

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
  (init-camera app (get-key app player:))
  (init-inputs app))

;;; ---------------------------------------------------------------------
;;; create the client
;;; ---------------------------------------------------------------------

(define (make-client #!optional (hub-name "Earth"))
  (let* ((client (FabricClient))
         (settings::AppSettings (get-key client settings:)))
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
