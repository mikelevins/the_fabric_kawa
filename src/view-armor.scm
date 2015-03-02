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
 make-absorb-armor
 make-regenerate-armor)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "syntax-classes.scm")
(require "data-assets.scm")
(require "appstate-character-creator.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as AssetManager com.jme3.asset.AssetManager)
(import-as ColorRGBA com.jme3.math.ColorRGBA)
(import-as Geometry com.jme3.scene.Geometry)
(import-as Material com.jme3.material.Material)
(import-as ParticleEmitter com.jme3.effect.ParticleEmitter)
(import-as ParticleMesh com.jme3.effect.ParticleMesh)
(import-as PrMaterial com.jme3.material.Material)
(import-as PrVector3f com.jme3.math.Vector3f)
(import-as RenderQueue com.jme3.renderer.queue.RenderQueue)
(import-as RenderState com.jme3.material.RenderState)
(import-as Sphere com.jme3.scene.shape.Sphere)

;;; ---------------------------------------------------------------------
;;; absorb armor
;;; ---------------------------------------------------------------------

(define (make-absorb-armor)
  (let* ((asset-manager (get-asset-manager))
         (sphere-mat::Material (Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
         (color (ColorRGBA 0.7 0.7 0.6 0.2)))
    (*:setColor sphere-mat "Color" color)
    (*:setColor sphere-mat "GlowColor" (ColorRGBA 1 1 0.9 0.1))
    (let* ((blendMode RenderState:BlendMode))
      (*:setBlendMode (*:getAdditionalRenderState sphere-mat) blendMode:Alpha))
    (let* ((new-sphere::Sphere (Sphere 32 32 3.5))
           (new-geom::Geometry (Geometry "CharacterArmor" new-sphere))
           (bucket RenderQueue:Bucket))
      (*:setMaterial new-geom sphere-mat)
      (*:setQueueBucket new-geom bucket:Transparent)
      (*:setLocalTranslation new-geom 0.0 0.0 0.0)
      new-geom)))


;;; ---------------------------------------------------------------------
;;; regenerate armor
;;; ---------------------------------------------------------------------

(define (make-regenerate-armor)
  (let* ((emitter (ParticleEmitter "Smoke" ParticleMesh:Type:Triangle 10))
         (mat (PrMaterial (get-asset-manager) "Common/MatDefs/Misc/Particle.j3md"))
         (start-color (ColorRGBA 0.3 0.8 0.3 0.3))
         (end-color (ColorRGBA 0.3 0.4 0.3 0.1))
         (v 0.25)
         (asset-manager::AssetManager (get-asset-manager)))
    (*:setTexture mat "Texture" (*:loadTexture asset-manager "Effects/Explosion/Debris.png"))
    (*:setMaterial emitter mat)
    (*:setImagesX emitter 3)
    (*:setImagesY emitter 3)
    (*:setEndColor emitter end-color)
    (*:setStartColor emitter start-color)
    (*:setInitialVelocity (*:getParticleInfluencer emitter)
                          (PrVector3f v v v))
    (*:setNumParticles emitter 16)
    (*:setParticlesPerSec emitter 16)
    (*:setSelectRandomImage emitter #t)
    (*:setRandomAngle emitter #t)
    (*:setStartSize emitter 4)
    (*:setEndSize emitter 0.1)
    (*:setGravity emitter 0 0 0)
    (*:setLowLife emitter 2)
    (*:setHighLife emitter 8)
    (*:setVelocityVariation (*:getParticleInfluencer emitter) 0.75)
    emitter))
