;;;; ***********************************************************************
;;;;
;;;; Name:          view-location-volvox.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       construct supported locations
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 make-volvox)

(require data-assets)

(import (only (com jme3 math FastMath) PI))

(import (class com.jme3.asset AssetManager))
(import (class com.jme3.effect ParticleEmitter ParticleMesh))
(import (class com.jme3.material Material))
(import (class com.jme3.material RenderState))
(import (class com.jme3.math ColorRGBA Quaternion Vector3f))
(import (class com.jme3.renderer.queue RenderQueue))
(import (class com.jme3.scene Geometry Node))
(import (class com.jme3.scene.shape Box Cylinder Quad Sphere))
(import (class com.jme3.texture Texture))
(import (class java.lang Class))

(define (make-sphere radius color::ColorRGBA glow-color::ColorRGBA)
  (let* ((asset-manager (get-asset-manager))
         (blendMode RenderState:BlendMode)
         (bucket RenderQueue:Bucket)
         (sphere::Sphere (Sphere 32 32 radius #t #t))
         (sphere-geom::Geometry (Geometry "Sphere" sphere))
         (sphere-mat::Material (Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
         (sphere-color (ColorRGBA 0.0 1.0 0.0 0.5))
         (sphere-glow-color (ColorRGBA 0.5 0.5 0.5 0.5)))
    (*:setColor sphere-mat "Color" color)
    (*:setColor sphere-mat "GlowColor" glow-color)
    (*:setBlendMode (*:getAdditionalRenderState sphere-mat) blendMode:Alpha)
    (*:setMaterial sphere-geom sphere-mat)
    (*:setQueueBucket sphere-geom bucket:Transparent)
    sphere-geom))

;;; an orbital city near Jupiter belonging to the Caretakers
(define (make-volvox-cell)
  (let* ((pivot (Node "Volvox Cell"))
         (asset-manager::AssetManager (get-asset-manager))
         (emitter::ParticleEmitter (ParticleEmitter "Smoke" ParticleMesh:Type:Triangle 32))
         (emitter-mat::Material (Material asset-manager "Common/MatDefs/Misc/Particle.j3md"))
         (emitter-start-color::ColorRGBA (ColorRGBA 0.3 0.8 0.3 0.4))
         (emitter-end-color::ColorRGBA (ColorRGBA 0.3 0.6 0.8 0.2))
         ;; sphere0 -- the central enclosing sphere
         (sphere0-geom::Geometry (make-sphere 256 (ColorRGBA 0.0 1.0 0.0 0.5)(ColorRGBA 0.5 0.5 0.5 0.5)))
         ;; sphere00 -- the center of the center
         (sphere00-geom::Geometry (make-sphere 84 (ColorRGBA 1.0 1.0 0.0 0.5)(ColorRGBA 0.5 0.5 0.5 0.5)))
         (sphere000-geom::Geometry (make-sphere 7 (ColorRGBA 1.0 1.0 1.0 1.0)(ColorRGBA 1.0 1.0 1.0 1.0)))
         ;; sphere01 -- the right-hand interior sphere
         (sphere01-geom::Geometry (make-sphere 84 (ColorRGBA 1.0 1.0 0.0 0.5)(ColorRGBA 0.5 0.5 0.5 0.5)))
         (sphere010-geom::Geometry (make-sphere 7 (ColorRGBA 1.0 1.0 1.0 1.0)(ColorRGBA 1.0 1.0 1.0 1.0)))
         ;; sphere02 -- the rear interior sphere
         (sphere02-geom::Geometry (make-sphere 84 (ColorRGBA 1.0 1.0 0.0 0.5)(ColorRGBA 0.5 0.5 0.5 0.5)))
         (sphere020-geom::Geometry (make-sphere 7 (ColorRGBA 1.0 1.0 1.0 1.0)(ColorRGBA 1.0 1.0 1.0 1.0)))
         ;; sphere03 -- the left interior sphere
         (sphere03-geom::Geometry (make-sphere 84 (ColorRGBA 1.0 1.0 0.0 0.5)(ColorRGBA 0.5 0.5 0.5 0.5)))
         (sphere030-geom::Geometry (make-sphere 7 (ColorRGBA 1.0 1.0 1.0 1.0)(ColorRGBA 1.0 1.0 1.0 1.0)))
         ;; sphere04 -- the front interior sphere
         (sphere04-geom::Geometry (make-sphere 84 (ColorRGBA 1.0 1.0 0.0 0.5)(ColorRGBA 0.5 0.5 0.5 0.5)))
         (sphere040-geom::Geometry (make-sphere 7 (ColorRGBA 1.0 1.0 1.0 1.0)(ColorRGBA 1.0 1.0 1.0 1.0)))
         ;; sphere05 -- the top interior sphere
         (sphere05-geom::Geometry (make-sphere 84 (ColorRGBA 1.0 1.0 0.0 0.5)(ColorRGBA 0.5 0.5 0.5 0.5)))
         (sphere050-geom::Geometry (make-sphere 7 (ColorRGBA 1.0 1.0 1.0 1.0)(ColorRGBA 1.0 1.0 1.0 1.0)))
         ;; sphere06 -- the bottom interior sphere
         (sphere06-geom::Geometry (make-sphere 84 (ColorRGBA 1.0 1.0 0.0 0.5)(ColorRGBA 0.5 0.5 0.5 0.5)))
         (sphere060-geom::Geometry (make-sphere 7 (ColorRGBA 1.0 1.0 1.0 1.0)(ColorRGBA 1.0 1.0 1.0 1.0))))
    (*:setLocalTranslation pivot 0.0 0.0 0.0)
    
    (*:attachChild pivot sphere0-geom)
    (*:setLocalTranslation sphere0-geom 0.0 0.0 0.0)    

    (*:attachChild pivot sphere00-geom)
    (*:setLocalTranslation sphere00-geom 0.0 0.0 0.0)
    (*:attachChild pivot sphere000-geom)
    (*:setLocalTranslation sphere000-geom 0.0 0.0 0.0)

    (*:attachChild pivot sphere01-geom)
    (*:setLocalTranslation sphere01-geom 168.0 0.0 0.0)
    (*:attachChild pivot sphere010-geom)
    (*:setLocalTranslation sphere010-geom 168.0 0.0 0.0)
    
    (*:attachChild pivot sphere02-geom)
    (*:setLocalTranslation sphere02-geom 0.0 0.0 -168.0)
    (*:attachChild pivot sphere020-geom)
    (*:setLocalTranslation sphere020-geom 0.0 0.0 -168.0)
    
    (*:attachChild pivot sphere03-geom)
    (*:setLocalTranslation sphere03-geom -168.0 0.0 0.0)
    (*:attachChild pivot sphere030-geom)
    (*:setLocalTranslation sphere030-geom -168.0 0.0 0.0)
    
    (*:attachChild pivot sphere04-geom)
    (*:setLocalTranslation sphere04-geom 0.0 0.0 168.0)
    (*:attachChild pivot sphere040-geom)
    (*:setLocalTranslation sphere040-geom 0.0 0.0 168.0)
    
    (*:attachChild pivot sphere05-geom)
    (*:setLocalTranslation sphere05-geom 0.0 168.0 0.0)
    (*:attachChild pivot sphere050-geom)
    (*:setLocalTranslation sphere050-geom 0.0 168.0 0.0)
    
    (*:attachChild pivot sphere06-geom)
    (*:setLocalTranslation sphere06-geom 0.0 -168.0 0.0)
    (*:attachChild pivot sphere060-geom)
    (*:setLocalTranslation sphere060-geom 0.0 -168.0 0.0)

    ;; the whole assembly
    (*:setTexture emitter-mat "Texture" (*:loadTexture asset-manager "Effects/Smoke/Smoke.png"))
    (*:setMaterial emitter emitter-mat)
    (*:setImagesX emitter 15)
    (*:setImagesY emitter 1)
    (*:setEndColor emitter emitter-end-color)
    (*:setStartColor emitter emitter-start-color)
    (*:setInitialVelocity (*:getParticleInfluencer emitter)
                          (Vector3f 2 1.4 1.6))
    (*:setNumParticles emitter 64)
    (*:setParticlesPerSec emitter 64)
    (*:setSelectRandomImage emitter #t)
    (*:setRandomAngle emitter #t)
    (*:setStartSize emitter 32)
    (*:setEndSize emitter 256)
    (*:setGravity emitter 0 0 0)
    (*:setLowLife emitter 64)
    (*:setHighLife emitter 256)
    (*:setVelocityVariation (*:getParticleInfluencer emitter) 2)
    (*:attachChild pivot emitter)

    pivot))

;;; an orbital city near Jupiter belonging to the Caretakers
(define (make-volvox)
  (let* ((pivot (Node "Volvox City"))
         (cell0::Node (make-volvox-cell))
         (cell1::Node (make-volvox-cell))
         (cell2::Node (make-volvox-cell))
         (cell3::Node (make-volvox-cell))
         (cell4::Node (make-volvox-cell))
         (cell5::Node (make-volvox-cell))
         (cell6::Node (make-volvox-cell)))
    (*:setLocalTranslation pivot 0.0 0.0 0.0)
    
    (*:attachChild pivot cell0)
    (*:setLocalTranslation cell0 0.0 0.0 0.0)    
    (*:attachChild pivot cell1)
    (*:setLocalTranslation cell1 512.0 0.0 0.0)    
    (*:attachChild pivot cell2)
    (*:setLocalTranslation cell2 0.0 0.0 -512.0)    
    (*:attachChild pivot cell3)
    (*:setLocalTranslation cell3 -512.0 0.0 0.0)    
    (*:attachChild pivot cell4)
    (*:setLocalTranslation cell4 0.0 0.0 512.0)    
    (*:attachChild pivot cell5)
    (*:setLocalTranslation cell5 0.0 512.0 0.0)    
    (*:attachChild pivot cell6)
    (*:setLocalTranslation cell6 0.0 -512.0 0.0)    

    pivot))
