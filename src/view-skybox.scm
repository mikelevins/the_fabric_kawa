;;;; ***********************************************************************
;;;;
;;;; Name:          view-skybox.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the skybox for game scenes
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 make-sky-box
 make-sky-sphere)

(require "util-java.scm")
(require "data-assets.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as AssetManager com.jme3.asset.AssetManager)
(import-as SimpleApplication com.jme3.app.SimpleApplication)
(import-as SkyFactory com.jme3.util.SkyFactory)

;;; ---------------------------------------------------------------------
;;; the skybox
;;; ---------------------------------------------------------------------

;;; (make-sky-box app::SimpleApplication)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed Fabric skybox

(define (make-sky-box app::SimpleApplication)
  (let ((asset-manager::AssetManager (get-asset-manager)))
    (SkyFactory:createSky asset-manager 
                          (*:loadTexture asset-manager "Textures/TychoSkymapII.t3_08192x04096_80_mx.jpg")
                          (*:loadTexture asset-manager "Textures/TychoSkymapII.t3_08192x04096_80_px.jpg")
                          (*:loadTexture asset-manager "Textures/TychoSkymapII.t3_08192x04096_80_pz.jpg")
                          (*:loadTexture asset-manager "Textures/TychoSkymapII.t3_08192x04096_80_mz.jpg")
                          (*:loadTexture asset-manager "Textures/TychoSkymapII.t3_08192x04096_80_py.jpg")
                          (*:loadTexture asset-manager "Textures/TychoSkymapII.t3_08192x04096_80_my.jpg"))))

;;; (make-sky-sphere app::SimpleApplication)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed Fabric skysphere

(define (make-sky-sphere app::SimpleApplication)
  (let ((asset-manager::AssetManager (get-asset-manager)))
    (SkyFactory:createSky asset-manager 
                          (*:loadTexture asset-manager "Textures/tycho2k.jpg")
                          #t)))
