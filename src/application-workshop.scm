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

(require "utilities-java.scm")
(require "interface-frame.scm")
(require "assets-general.scm")
(require "application-common.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(define-private-alias AssetManager com.jme3.asset.AssetManager)
(define-private-alias BloomFilter com.jme3.post.filters.BloomFilter)
(define-private-alias FilterPostProcessor com.jme3.post.FilterPostProcessor)
(define-private-alias Geometry com.jme3.scene.Geometry)
(define-private-alias Mouse org.lwjgl.input.Mouse)
(define-private-alias SkyFactory com.jme3.util.SkyFactory)
(define-private-alias Spatial com.jme3.scene.Spatial)

;;; ---------------------------------------------------------------------
;;; FabricApp - the abstract client application class
;;; ---------------------------------------------------------------------

(define-simple-class FabricWorkshop (FabricApp)
  )

;;; ---------------------------------------------------------------------
;;; application initialization
;;; ---------------------------------------------------------------------

(define (setup-lighting app ::FabricWorkshop)
  (let* ((asset-manager::AssetManager (get-asset-manager))
         (bloom (BloomFilter BloomFilter:GlowMode:Objects))
         (filter-processor::FilterPostProcessor (FilterPostProcessor asset-manager))
         (viewport (get-key app viewport:)))
    (*:setDownSamplingFactor bloom 2.0)
    (*:setBloomIntensity bloom 2.0)
    (*:addFilter filter-processor bloom)
    (*:addProcessor viewport filter-processor)))

(define (make-workshop-sky)
  (let* ((asset-manager::AssetManager (get-asset-manager))
         (sky::Spatial (SkyFactory:createSky asset-manager 
                                             (*:loadTexture asset-manager "Textures/boxgrid.jpg")
                                             (*:loadTexture asset-manager "Textures/boxgrid.jpg")
                                             (*:loadTexture asset-manager "Textures/boxgrid.jpg")
                                             (*:loadTexture asset-manager "Textures/boxgrid.jpg")
                                             (*:loadTexture asset-manager "Textures/boxgrid.jpg")
                                             (*:loadTexture asset-manager "Textures/boxgrid.jpg"))))
    
    (*:setName sky "skybox")
    sky))

(define (init-workshop app)
  (let ((default-sky (make-workshop-sky))
        (fly-cam (get-key app flyby-camera:)))
    (setup-lighting app)
    (set-key! app skybox: default-sky)
    ;; don't seize the mouse from the user
    (Mouse:setGrabbed #f)
    (*:setDragToRotate fly-cam #t)
    (*:setMoveSpeed fly-cam 100)))

(define (make-workshop)
  (let ((shop (FabricWorkshop)))
    (set-key! shop application-init: init-workshop)
    shop))
