;;;; ***********************************************************************
;;;;
;;;; Name:          view-augments.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the display of character augments
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 make-force-augment
 make-optics-augment
 make-portals-augment
 make-turrets-augment)



;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "syntax-classes.scm")
(require "data-assets.scm")
(require "appstate-character-creator.scm")
(require "view-controls.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as AssetManager com.jme3.asset.AssetManager)
(import-as ColorRGBA com.jme3.math.ColorRGBA)
(import-as Cylinder com.jme3.scene.shape.Cylinder)
(import-as Geometry com.jme3.scene.Geometry)
(import-as Material com.jme3.material.Material)
(import-as Node com.jme3.scene.Node)
(import-as PI com.jme3.math.FastMath:PI)
(import-as ParticleEmitter com.jme3.effect.ParticleEmitter)
(import-as ParticleMesh com.jme3.effect.ParticleMesh)
(import-as PrMaterial com.jme3.material.Material)
(import-as PrVector3f com.jme3.math.Vector3f)
(import-as Quaternion com.jme3.math.Quaternion)
(import-as RenderQueue com.jme3.renderer.queue.RenderQueue)
(import-as RenderState com.jme3.material.RenderState)
(import-as Sphere com.jme3.scene.shape.Sphere)
(import-as Torus com.jme3.scene.shape.Torus)
(import-as Vector3f com.jme3.math.Vector3f)
(import-as WireBox com.jme3.scene.debug.WireBox)
(import-as WireSphere com.jme3.scene.debug.WireSphere)

;;; ---------------------------------------------------------------------
;;; force fields
;;; ---------------------------------------------------------------------

(define (make-force-augment)
  (let* ((asset-manager (get-asset-manager))
         (force-mat::Material (Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
         (color (ColorRGBA 0.8 0.8 1 0.75))
         (glow-color (ColorRGBA 0.8 0.8 1 0.9))
         (pivot (Node "CharacterAugment"))
         (pivot0 (Node "ModulePivot0"))
         (pivot1 (Node "ModulePivot1")))
    (*:setColor force-mat "Color" color)
    (*:setColor force-mat "GlowColor" glow-color)
    (let* ((blendMode RenderState:BlendMode))
      (*:setBlendMode (*:getAdditionalRenderState force-mat) blendMode:Alpha))
    (let* ((force-module0 (Cylinder 6 6 0.125 0.75 #t))
           (force-module1 (Cylinder 6 6 0.125 0.75 #t))
           (geom0::Geometry (Geometry "Module0" force-module0))
           (geom1::Geometry (Geometry "Module1" force-module1))
           (bucket RenderQueue:Bucket)
           (rotation::Quaternion (Quaternion))
           (pitch-axis (Vector3f 1 0 0)))
      (*:fromAngleAxis rotation (/ PI 2) pitch-axis)
      (*:setLocalRotation geom0 rotation)
      (*:setLocalRotation geom1 rotation)
      (*:setMaterial geom0 force-mat)
      (*:setMaterial geom1 force-mat)
      (*:setQueueBucket geom0 bucket:Transparent)
      (*:setQueueBucket geom1 bucket:Transparent)
      (*:setLocalTranslation geom0 0.0 0.0 0.0)
      (*:setLocalTranslation geom1 0.0 0.0 0.0)
      (*:setLocalTranslation pivot 0.0 0.0 0.0)
      (*:setLocalTranslation pivot0 0.0 4.5 0.0)
      (*:setLocalTranslation pivot1 0.0 -4.5 0.0)
      (*:attachChild pivot0 geom0)
      (*:attachChild pivot1 geom1)
      (*:attachChild pivot pivot0)
      (*:attachChild pivot pivot1)
      pivot)))


;;; ---------------------------------------------------------------------
;;; optics
;;; ---------------------------------------------------------------------

(define (make-optics-augment)
  (let* ((asset-manager (get-asset-manager))
         (mat::Material (Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
         (color (ColorRGBA 0.5 0 0.7 0.25))
         (glow-color (ColorRGBA 0.8 0.8 1 1))
         (pivot (Node "CharacterWeapon"))
         (pivot0 (Node "ModulePivot0"))
         (pivot1 (Node "ModulePivot1")))
    (*:setColor mat "Color" color)
    (*:setColor mat "GlowColor" glow-color)
    (let* ((blendMode RenderState:BlendMode))
      (*:setBlendMode (*:getAdditionalRenderState mat) blendMode:Alpha))
    (let* ((torus0::Torus (Torus 32 32 0.05 0.25))
           (sphere0::Sphere (Sphere 32 32 0.15))
           (torus1::Torus (Torus 32 32 0.05 0.25))
           (sphere1::Sphere (Sphere 32 32 0.15))
           (geom0::Geometry (Geometry "Module0" torus0))
           (geom1::Geometry (Geometry "Module1" sphere0))
           (geom2::Geometry (Geometry "Module2" torus0))
           (geom3::Geometry (Geometry "Module3" sphere0))
           (bucket RenderQueue:Bucket))
      (*:setMaterial geom0 mat)
      (*:setMaterial geom1 mat)
      (*:setMaterial geom2 mat)
      (*:setMaterial geom3 mat)
      (*:setQueueBucket geom0 bucket:Transparent)
      (*:setQueueBucket geom1 bucket:Transparent)
      (*:setQueueBucket geom2 bucket:Transparent)
      (*:setQueueBucket geom3 bucket:Transparent)
      (*:setLocalTranslation geom0 0.0 0.0 0.0)
      (*:setLocalTranslation geom1 0.0 0.0 0.0)
      (let ((rotation (Quaternion))
            (pitch-axis (Vector3f 1 0 0)))
        (*:fromAngleAxis rotation PI pitch-axis)
        (*:setLocalRotation geom0 rotation))
      (*:setLocalTranslation geom2 0.0 0.0 0.0)
      (*:setLocalTranslation geom3 0.0 0.0 0.0)
      (let ((rotation (Quaternion))
            (pitch-axis (Vector3f 1 0 0)))
        (*:fromAngleAxis rotation PI pitch-axis)
        (*:setLocalRotation geom2 rotation))
      (*:setLocalTranslation pivot 0.0 0.0 0.0)
      (*:setLocalTranslation pivot0 0.0 4.5 0.0)
      (*:setLocalTranslation pivot1 0.0 -4.5 0.0)
      (*:attachChild pivot0 geom0)
      (*:attachChild pivot0 geom1)
      (*:attachChild pivot1 geom2)
      (*:attachChild pivot1 geom3)
      (*:attachChild pivot pivot0)
      (*:attachChild pivot pivot1)
      pivot)))

;;; ---------------------------------------------------------------------
;;; power armor
;;; ---------------------------------------------------------------------

(define (make-portals-augment)
  #!null)


;;; ---------------------------------------------------------------------
;;; power armor
;;; ---------------------------------------------------------------------

(define (make-turrets-augment)
  #!null)


