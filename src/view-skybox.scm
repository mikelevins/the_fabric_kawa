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
 make-sky-box)

(require util-java)
(require data-assets)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as AssetManager com.jme3.asset.AssetManager)
(import-as SkyFactory com.jme3.util.SkyFactory)

;;; ---------------------------------------------------------------------
;;; the skybox
;;; ---------------------------------------------------------------------

;;; (make-sky-box)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed Fabric skybox

(define (make-sky-box)
  (let ((asset-manager::AssetManager (get-asset-manager)))
    (SkyFactory:createSky asset-manager 
                          (*:loadTexture asset-manager "Textures/TychoSkymapII.t3_08192x04096_80_mx.jpg")
                          (*:loadTexture asset-manager "Textures/TychoSkymapII.t3_08192x04096_80_px.jpg")
                          (*:loadTexture asset-manager "Textures/TychoSkymapII.t3_08192x04096_80_mz.jpg")
                          (*:loadTexture asset-manager "Textures/TychoSkymapII.t3_08192x04096_80_pz.jpg")
                          (*:loadTexture asset-manager "Textures/TychoSkymapII.t3_08192x04096_80_py.jpg")
                          (*:loadTexture asset-manager "Textures/TychoSkymapII.t3_08192x04096_80_my.jpg"))))
