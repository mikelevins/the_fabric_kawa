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
         (color (ColorRGBA 0.6 0 0 0.7))
         (glow-color (ColorRGBA 0.9 0.4 0.4 0.9))
         (pivot (Node "CharacterWeapon"))
         (pivot0 (Node "ModulePivot0"))
         (pivot1 (Node "ModulePivot1")))
    (*:setColor cannon-mat "Color" color)
    (*:setColor cannon-mat "GlowColor" glow-color)
    (let* ((blendMode RenderState:BlendMode))
      (*:setBlendMode (*:getAdditionalRenderState cannon-mat) blendMode:Alpha))
    (let* ((cannon-module0 (Dome (Vector3f 0 0 0) 2 4 0.25 #f))
           (cannon-module1 (Dome (Vector3f 0 0 0) 2 4 0.25 #f))
           (cannon-module2 (Dome (Vector3f 0 0 0) 2 4 0.25 #f))
           (cannon-module3 (Dome (Vector3f 0 0 0) 2 4 0.25 #f))
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
  (let* ((pivot (Node "CharacterWeapon"))
         (pivot0 (Node "ModulePivot0"))
         (pivot1 (Node "ModulePivot1"))
         (emitter0 (ParticleEmitter "Smoke" ParticleMesh:Type:Triangle 32))
         (emitter1 (ParticleEmitter "Smoke" ParticleMesh:Type:Triangle 32))
         (asset-manager::AssetManager (get-asset-manager))
         (mat (PrMaterial (get-asset-manager) "Common/MatDefs/Misc/Particle.j3md"))
         (ball-mat::Material (Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
         (start-color (ColorRGBA 0.5 0.2 0.8 0.5))
         (end-color (ColorRGBA 0.1 0.2 0.3 0.4))
         (ball-color (ColorRGBA 0.3 0.0 0.4 0.4))
         (glow-color (ColorRGBA 0.3 0.0 0.4 0.8)))
    (*:setColor ball-mat "Color" ball-color)
    (*:setColor ball-mat "GlowColor" glow-color)
    (let* ((blendMode RenderState:BlendMode)
           (ball-module0 (Sphere 32 32 0.125))
           (ball-module1 (Sphere 32 32 0.125))
           (geom0::Geometry (Geometry "Module0" ball-module0))
           (geom1::Geometry (Geometry "Module1" ball-module1))
           (bucket RenderQueue:Bucket))
      (*:setBlendMode (*:getAdditionalRenderState ball-mat) blendMode:Alpha)
      (*:setTexture mat "Texture" (*:loadTexture asset-manager "Effects/Explosion/Debris.png"))
      (*:setMaterial geom0 ball-mat)
      (*:setMaterial geom1 ball-mat)
      (*:setQueueBucket geom0 bucket:Transparent)
      (*:setQueueBucket geom1 bucket:Transparent)
      (*:setMaterial emitter0 mat)
      (*:setMaterial emitter1 mat)
      (*:setImagesX emitter0 3)
      (*:setImagesX emitter1 3)
      (*:setImagesY emitter0 3)
      (*:setImagesY emitter1 3)
      (*:setStartColor emitter0 start-color)
      (*:setStartColor emitter1 start-color)
      (*:setEndColor emitter0 end-color)
      (*:setEndColor emitter1 end-color)
      (*:setInitialVelocity (*:getParticleInfluencer emitter0)
                            (PrVector3f 0.4 0.4 0.5))
      (*:setInitialVelocity (*:getParticleInfluencer emitter1)
                            (PrVector3f 0.4 0.5 0.4))
      (*:setNumParticles emitter0 64)
      (*:setNumParticles emitter1 64)
      (*:setParticlesPerSec emitter0 64)
      (*:setParticlesPerSec emitter1 64)
      (*:setSelectRandomImage emitter0 #t)
      (*:setSelectRandomImage emitter1 #t)
      (*:setRandomAngle emitter0 #t)
      (*:setRandomAngle emitter1 #t)
      (*:setStartSize emitter0 0.25)
      (*:setStartSize emitter1 0.25)
      (*:setEndSize emitter0 0.1)
      (*:setEndSize emitter1 0.1)
      (*:setGravity emitter0 0 0 0)
      (*:setGravity emitter1 0 0 0)
      (*:setLowLife emitter0 2)
      (*:setLowLife emitter1 2)
      (*:setHighLife emitter0 16)
      (*:setHighLife emitter1 16)
      (*:setVelocityVariation (*:getParticleInfluencer emitter0) 0.5)
      (*:setVelocityVariation (*:getParticleInfluencer emitter1) 0.5)
      (*:setLocalTranslation geom0 0.0 0.0 0.0)
      (*:setLocalTranslation geom1 0.0 0.0 0.0)
      (*:setLocalTranslation pivot 0.0 0.0 0.0)
      (*:setLocalTranslation pivot0 4.5 0.0 0.0)
      (*:setLocalTranslation pivot1 -4.5 0.0 0.0)
      (*:attachChild pivot0 emitter0)
      (*:attachChild pivot1 emitter1)
      (*:attachChild pivot0 geom0)
      (*:attachChild pivot1 geom1)
      (*:attachChild pivot pivot0)
      (*:attachChild pivot pivot1)
      pivot)))

;;; ---------------------------------------------------------------------
;;; bots
;;; ---------------------------------------------------------------------

(define (make-bots-weapons)
  #!null)


