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

(defmethod appstate ((app FabricWorkshop) (key (singleton app-settings:)))
  (*:getAppSettings app))

(defmethod appstate ((app FabricWorkshop) (key (singleton root-node:)))
  (*:getRootNode app))

(defmethod appstate ((app FabricWorkshop) (key (singleton application-init:)))
  (get-key (*:getFrameState app)
           application-init: default-init-application))

(defmethod appstate ((app FabricWorkshop) (key (singleton viewport:)))
  (*:getViewPort app))

(defmethod set-appstate! ((app FabricWorkshop) (key (singleton application-init:)) (initfn gnu.expr.ModuleMethod))
  (set-key! (*:getFrameState app) application-init: initfn))

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
        (root (appstate app root-node:)))
    (setup-lighting app)
    (*:attachChild root default-sky)
    ))

(define (make-workshop)
  (let ((shop (FabricWorkshop)))
    (set-appstate! shop application-init: init-workshop)
    shop))
