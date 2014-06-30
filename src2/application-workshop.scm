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
(require "language-types.scm")
(require "language-gf.scm")
(require "model-frames.scm")
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


;;; ---------------------------------------------------------------------
;;; FabricApp - the abstract client application class
;;; ---------------------------------------------------------------------

(define-simple-class FabricWorkshop (FabricApp)
  ;; slots
  ;; -------

  ;; accessors
  ;; ---------

  ;; implementation methods
  ;; ---------
)

;;; ---------------------------------------------------------------------
;;; accessor functions
;;; ---------------------------------------------------------------------

;;; getters
;;; ---------------------------------------------------------------------

;;; app-settings:
(defmethod appstate ((app FabricWorkshop) (key (singleton app-settings:)))
  (*:getAppSettings app))

;;; root-node:
(defmethod appstate ((app FabricWorkshop) (key (singleton root-node:)))
  (*:getRootNode app))

;;; application-init:
(defmethod appstate ((app FabricWorkshop) (key (singleton application-init:)))
  (get-key (*:getFrameState app)
           application-init: default-init-application))

;;; viewport:
(defmethod appstate ((app FabricWorkshop) (key (singleton viewport:)))
  (*:getViewPort app))

;;; sky:
(defmethod appstate ((app FabricWorkshop) (key (singleton sky:)))
  (get-key (*:getFrameState app)
           sky: #f))

;;; fly-by-camera:
(defmethod appstate ((app FabricWorkshop) (key (singleton fly-by-camera:)))
  (*:getFlyByCamera app))

;;; setters
;;; ---------------------------------------------------------------------

;;; application-init:
(defmethod set-appstate! ((app FabricWorkshop) (key (singleton application-init:)) (initfn gnu.expr.ModuleMethod))
  (set-key! (*:getFrameState app) application-init: initfn)
  app)

;;; sky:
(defmethod set-appstate! ((app FabricWorkshop) (key (singleton sky:)) (sky Geometry))
  (set-key! (*:getFrameState app) sky: sky)
  (let ((root (appstate app root-node:)))
    (*:attachChild root sky))
  app)

;;; ---------------------------------------------------------------------
;;; application initialization
;;; ---------------------------------------------------------------------

(define (setup-lighting app ::FabricWorkshop)
  (let* ((asset-manager::AssetManager (get-asset-manager))
         (bloom (BloomFilter BloomFilter:GlowMode:Objects))
         (filter-processor::FilterPostProcessor (FilterPostProcessor asset-manager))
         (viewport (appstate app viewport:)))
    (*:setDownSamplingFactor bloom 2.0)
    (*:setBloomIntensity bloom 2.0)
    (*:addFilter filter-processor bloom)
    (*:addProcessor viewport filter-processor)))

(define (make-workshop-sky app :: FabricWorkshop)
  (let ((asset-manager::AssetManager (get-asset-manager)))
    (SkyFactory:createSky asset-manager 
                          (*:loadTexture asset-manager "Textures/boxgrid.jpg")
                          (*:loadTexture asset-manager "Textures/boxgrid.jpg")
                          (*:loadTexture asset-manager "Textures/boxgrid.jpg")
                          (*:loadTexture asset-manager "Textures/boxgrid.jpg")
                          (*:loadTexture asset-manager "Textures/boxgrid.jpg")
                          (*:loadTexture asset-manager "Textures/boxgrid.jpg"))))

(define (init-workshop app)
  (let ((default-sky (make-workshop-sky app))
        (fly-cam (appstate app fly-by-camera:)))
    (setup-lighting app)
    (set-appstate! app sky: default-sky)
    ;; don't seize the mouse from the user
    (Mouse:setGrabbed #f)
    (*:setDragToRotate fly-cam #t)
    (*:setMoveSpeed fly-cam 100)))

(define (make-workshop)
  (let ((shop (FabricWorkshop)))
    (set-appstate! shop application-init: init-workshop)
    shop))
