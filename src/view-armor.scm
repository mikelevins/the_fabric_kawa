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
 make-energy-armor
 make-power-armor
 make-regenerate-armor)

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
(import-as Geometry com.jme3.scene.Geometry)
(import-as Material com.jme3.material.Material)
(import-as Node com.jme3.scene.Node)
(import-as ParticleEmitter com.jme3.effect.ParticleEmitter)
(import-as ParticleMesh com.jme3.effect.ParticleMesh)
(import-as PrMaterial com.jme3.material.Material)
(import-as PrVector3f com.jme3.math.Vector3f)
(import-as RenderQueue com.jme3.renderer.queue.RenderQueue)
(import-as RenderState com.jme3.material.RenderState)
(import-as Sphere com.jme3.scene.shape.Sphere)
(import-as Torus com.jme3.scene.shape.Torus)
(import-as WireBox com.jme3.scene.debug.WireBox)
(import-as WireSphere com.jme3.scene.debug.WireSphere)

;;; ---------------------------------------------------------------------
;;; absorb armor
;;; ---------------------------------------------------------------------

(define (make-absorb-armor)
  (let* ((asset-manager (get-asset-manager))
         (sphere-mat::Material (Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
         (sphere-color (ColorRGBA 1 1 0.9 0.05))
         (sphere-glow-color (ColorRGBA 0.8 0.9 1 0.5)))
    (*:setColor sphere-mat "Color" sphere-color)
    (*:setColor sphere-mat "GlowColor" sphere-glow-color)
    (let* ((blendMode RenderState:BlendMode))
      (*:setBlendMode (*:getAdditionalRenderState sphere-mat) blendMode:Alpha))
    (let* ((pivot (Node "CharacterArmor"))
           (sphere::Sphere (Sphere 32 32 3.75))
           (sphere-geom::Geometry (Geometry "Sphere" sphere))
           (bucket RenderQueue:Bucket))
      (*:setMaterial sphere-geom sphere-mat)
      (*:setQueueBucket sphere-geom bucket:Transparent)
      (*:setLocalTranslation sphere-geom 0.0 0.0 0.0)
      (*:setLocalTranslation pivot 0.0 0.0 0.0)
      (*:attachChild pivot sphere-geom)
      pivot)))


;;; ---------------------------------------------------------------------
;;; regenerate armor
;;; ---------------------------------------------------------------------

(define (make-regenerate-armor)
  (let* ((emitter (ParticleEmitter "Smoke" ParticleMesh:Type:Triangle 32))
         (mat (PrMaterial (get-asset-manager) "Common/MatDefs/Misc/Particle.j3md"))
         (start-color (ColorRGBA 0.3 0.8 0.3 0.3))
         (end-color (ColorRGBA 0.3 0.4 0.3 0.1))
         (asset-manager::AssetManager (get-asset-manager)))
    (*:setTexture mat "Texture" (*:loadTexture asset-manager "Effects/Smoke/Smoke.png"))
    (*:setMaterial emitter mat)
    (*:setImagesX emitter 15)
    (*:setImagesY emitter 1)
    (*:setEndColor emitter end-color)
    (*:setStartColor emitter start-color)
    (*:setInitialVelocity (*:getParticleInfluencer emitter)
                          (PrVector3f 0.4 0.4 0.5))
    (*:setNumParticles emitter 32)
    (*:setParticlesPerSec emitter 32)
    (*:setSelectRandomImage emitter #t)
    (*:setRandomAngle emitter #t)
    (*:setStartSize emitter 3)
    (*:setEndSize emitter 0.1)
    (*:setGravity emitter 0 0 0)
    (*:setLowLife emitter 2)
    (*:setHighLife emitter 16)
    (*:setVelocityVariation (*:getParticleInfluencer emitter) 0.6)
    emitter))

;;; ---------------------------------------------------------------------
;;; power armor
;;; ---------------------------------------------------------------------

(define (make-power-armor)
  (let* ((asset-manager (get-asset-manager))
         (torus-mat::Material (Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
         (color (ColorRGBA 1 0 0 0.2))
         (glow-color (ColorRGBA 1 0.2 0.2 0.75))
         (pivot (Node "CharacterArmor"))
         (ring-pivot0 (Node "RingPivot0"))
         (ring-pivot1 (Node "RingPivot1")))
    (*:setColor torus-mat "Color" color)
    (*:setColor torus-mat "GlowColor" glow-color)
    (let* ((blendMode RenderState:BlendMode))
      (*:setBlendMode (*:getAdditionalRenderState torus-mat) blendMode:Alpha))
    (let* ((torus0::Torus (Torus 32 32 0.1 3.75))
           (rotator0 (make-rotator-control 2 0 0))
           (torus1::Torus (Torus 32 32 0.1 3.75))
           (rotator1 (make-rotator-control -2 0 0))
           (geom0::Geometry (Geometry "Ring0" torus0))
           (geom1::Geometry (Geometry "Ring1" torus1))
           (bucket RenderQueue:Bucket))
      (*:setMaterial geom0 torus-mat)
      (*:setMaterial geom1 torus-mat)
      (*:setQueueBucket geom0 bucket:Transparent)
      (*:setQueueBucket geom1 bucket:Transparent)
      (*:setLocalTranslation geom0 0.0 0.0 0.0)
      (*:setLocalTranslation geom1 0.0 0.0 0.0)
      (*:setLocalTranslation pivot 0.0 0.0 0.0)
      (*:setLocalTranslation ring-pivot0 0.0 0.0 0.0)
      (*:setLocalTranslation ring-pivot1 0.0 0.0 0.0)
      (*:attachChild ring-pivot0 geom0)
      (*:addControl (as Node ring-pivot0) rotator0)
      (*:attachChild ring-pivot1 geom1)
      (*:addControl (as Node ring-pivot1) rotator1)
      (*:attachChild pivot ring-pivot0)
      (*:attachChild pivot ring-pivot1)
      pivot)))


;;; ---------------------------------------------------------------------
;;; power armor
;;; ---------------------------------------------------------------------

(define (make-energy-armor)
  (let* ((asset-manager (get-asset-manager))
         (sphere-mat::Material (Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
         (sphere-color (ColorRGBA 0.9 0.9 1 0.5))
         (sphere-glow-color (ColorRGBA 0.8 0.9 1 1)))
    (*:setColor sphere-mat "Color" sphere-color)
    (*:setColor sphere-mat "GlowColor" sphere-glow-color)
    (let* ((blendMode RenderState:BlendMode))
      (*:setBlendMode (*:getAdditionalRenderState sphere-mat) blendMode:Alpha))
    (let* ((pivot (Node "CharacterArmor"))
           (sphere::WireSphere (WireSphere 3.75))
           (sphere-geom::Geometry (Geometry "Sphere" sphere))
           (bucket RenderQueue:Bucket))
      (*:setMaterial sphere-geom sphere-mat)
      (*:setQueueBucket sphere-geom bucket:Transparent)
      (*:setLocalTranslation sphere-geom 0.0 0.0 0.0)
      (*:setLocalTranslation pivot 0.0 0.0 0.0)
      (*:attachChild pivot sphere-geom)
      pivot)))


