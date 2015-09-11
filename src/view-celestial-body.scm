;;;; ***********************************************************************
;;;;
;;;; Name:          view-celestial-body.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       utilities for constructing celestial bodies
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 make-celestial-body)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "data-assets.scm")
(require "view-rotatecontrol.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as AssetManager com.jme3.asset.AssetManager)
(import-as Sphere com.jme3.scene.shape.Sphere)
(import-as Material com.jme3.material.Material)
(import-as Geometry com.jme3.scene.Geometry)
(import-as Quaternion com.jme3.math.Quaternion)
(import-as Vector3f com.jme3.math.Vector3f)
(import-as PI com.jme3.math.FastMath:PI)

;;; ---------------------------------------------------------------------
;;; building celestial bodies
;;; ---------------------------------------------------------------------

(define (make-celestial-body texture-name)
  (let* ((asset-manager::AssetManager (get-asset-manager))
         (celestial-body::Sphere (Sphere 128 128 4096.0))
         (body-mat::Material (Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
         (body-texture (*:loadTexture asset-manager (string-append "Textures/" texture-name)))
         (body-pivot::Geometry (Geometry texture-name celestial-body))
         (body-rotator::RotateControl (RotateControl 0.0 0.0 0.025))
         (rotation (Quaternion))
         (TextureMode Sphere:TextureMode)
         (Projected TextureMode:Projected)
         (pitch-axis (Vector3f 1 0 0)))
    (*:fromAngleAxis rotation (* -1 (/ PI 2)) pitch-axis)
    (*:setLocalRotation body-pivot rotation)
    (*:setLocalTranslation body-pivot 0 0 0)
    (*:setTextureMode celestial-body Projected)
    (*:setTexture body-mat "ColorMap" body-texture)
    (*:setMaterial body-pivot body-mat)
    (*:addControl body-pivot body-rotator)
    body-pivot))
