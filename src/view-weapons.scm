;;;; ***********************************************************************
;;;;
;;;; Name:          view-armor.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the display of character armor
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 make-bots-weapons
 make-cannon-weapons
 make-impulse-weapons
 make-malware-weapons)

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
(import-as Dome com.jme3.scene.shape.Dome)
(import-as Geometry com.jme3.scene.Geometry)
(import-as Material com.jme3.material.Material)
(import-as Node com.jme3.scene.Node)
(import-as ParticleEmitter com.jme3.effect.ParticleEmitter)
(import-as ParticleMesh com.jme3.effect.ParticleMesh)
(import-as PI com.jme3.math.FastMath:PI)
(import-as PrMaterial com.jme3.material.Material)
(import-as PrVector3f com.jme3.math.Vector3f)
(import-as Quaternion com.jme3.math.Quaternion)
(import-as RenderQueue com.jme3.renderer.queue.RenderQueue)
(import-as RenderState com.jme3.material.RenderState)
(import-as Sphere com.jme3.scene.shape.Sphere)
(import-as Vector3f com.jme3.math.Vector3f)
(import-as WireBox com.jme3.scene.debug.WireBox)
(import-as WireSphere com.jme3.scene.debug.WireSphere)

;;; ---------------------------------------------------------------------
;;; cannons
;;; ---------------------------------------------------------------------

(define (make-cannon-weapons)
  (let* ((asset-manager (get-asset-manager))
         (cannon-mat::Material (Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
         (color (ColorRGBA 0.6 0 0 0.8))
         (glow-color (ColorRGBA 0.8 0.2 0.2 0.9))
         (pivot (Node "CharacterWeapon"))
         (pivot0 (Node "ModulePivot0"))
         (pivot1 (Node "ModulePivot1")))
    (*:setColor cannon-mat "Color" color)
    (*:setColor cannon-mat "GlowColor" glow-color)
    (let* ((blendMode RenderState:BlendMode))
      (*:setBlendMode (*:getAdditionalRenderState cannon-mat) blendMode:Alpha))
    (let* ((cannon-module0 (Dome (Vector3f 0 0 0) 2 4 0.5 #f))
           (cannon-module1 (Dome (Vector3f 0 0 0) 2 4 0.5 #f))
           (cannon-module2 (Dome (Vector3f 0 0 0) 2 4 0.5 #f))
           (cannon-module3 (Dome (Vector3f 0 0 0) 2 4 0.5 #f))
           (geom0::Geometry (Geometry "Module0" cannon-module0))
           (geom1::Geometry (Geometry "Module1" cannon-module1))
           (geom2::Geometry (Geometry "Module2" cannon-module2))
           (geom3::Geometry (Geometry "Module3" cannon-module3))
           (bucket RenderQueue:Bucket))
      (*:setMaterial geom0 cannon-mat)
      (*:setMaterial geom1 cannon-mat)
      (*:setMaterial geom2 cannon-mat)
      (*:setMaterial geom3 cannon-mat)
      (*:setQueueBucket geom0 bucket:Transparent)
      (*:setQueueBucket geom1 bucket:Transparent)
      (*:setQueueBucket geom2 bucket:Transparent)
      (*:setQueueBucket geom3 bucket:Transparent)
      (*:setLocalTranslation geom0 0.0 0.0 0.0)
      (*:setLocalTranslation geom1 0.0 0.0 0.0)
      (let ((rotation (Quaternion))
            (pitch-axis (Vector3f 1 0 0)))
        (*:fromAngleAxis rotation PI pitch-axis)
        (*:setLocalRotation geom1 rotation))
      (*:setLocalTranslation geom2 0.0 0.0 0.0)
      (*:setLocalTranslation geom3 0.0 0.0 0.0)
      (let ((rotation (Quaternion))
            (pitch-axis (Vector3f 1 0 0)))
        (*:fromAngleAxis rotation PI pitch-axis)
        (*:setLocalRotation geom3 rotation))
      (*:setLocalTranslation pivot 0.0 0.0 0.0)
      (*:setLocalTranslation pivot0 4.5 0.0 0.0)
      (*:setLocalTranslation pivot1 -4.5 0.0 0.0)
      (*:attachChild pivot0 geom0)
      (*:attachChild pivot0 geom1)
      (*:attachChild pivot1 geom2)
      (*:attachChild pivot1 geom3)
      (*:attachChild pivot pivot0)
      (*:attachChild pivot pivot1)
      pivot)))

;;; ---------------------------------------------------------------------
;;; impulse weapons
;;; ---------------------------------------------------------------------

(define (make-impulse-weapons)
  (let* ((asset-manager (get-asset-manager))
         (impulse-mat::Material (Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
         (color (ColorRGBA 0.8 0.8 1 0.75))
         (glow-color (ColorRGBA 0.8 0.8 1 0.9))
         (pivot (Node "CharacterWeapon"))
         (pivot0 (Node "ModulePivot0"))
         (pivot1 (Node "ModulePivot1")))
    (*:setColor impulse-mat "Color" color)
    (*:setColor impulse-mat "GlowColor" glow-color)
    (let* ((blendMode RenderState:BlendMode))
      (*:setBlendMode (*:getAdditionalRenderState impulse-mat) blendMode:Alpha))
    (let* ((impulse-module0 (Sphere 32 32 0.25))
           (impulse-module1 (Sphere 32 32 0.25))
           (geom0::Geometry (Geometry "Module0" impulse-module0))
           (geom1::Geometry (Geometry "Module1" impulse-module1))
           (bucket RenderQueue:Bucket))
      (*:setMaterial geom0 impulse-mat)
      (*:setMaterial geom1 impulse-mat)
      (*:setQueueBucket geom0 bucket:Transparent)
      (*:setQueueBucket geom1 bucket:Transparent)
      (*:setLocalTranslation geom0 0.0 0.0 0.0)
      (*:setLocalTranslation geom1 0.0 0.0 0.0)
      (*:setLocalTranslation pivot 0.0 0.0 0.0)
      (*:setLocalTranslation pivot0 4.5 0.0 0.0)
      (*:setLocalTranslation pivot1 -4.5 0.0 0.0)
      (*:attachChild pivot0 geom0)
      (*:attachChild pivot1 geom1)
      (*:attachChild pivot pivot0)
      (*:attachChild pivot pivot1)
      pivot)))

;;; ---------------------------------------------------------------------
;;; malware
;;; ---------------------------------------------------------------------

(define (make-malware-weapons)
  #!null)

;;; ---------------------------------------------------------------------
;;; bots
;;; ---------------------------------------------------------------------

(define (make-bots-weapons)
  #!null)


