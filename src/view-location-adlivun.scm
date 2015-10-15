;;;; ***********************************************************************
;;;;
;;;; Name:          view-location-adlivun.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       construct supported locations
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 make-adlivun)

(require data-assets)
(require view-rotatecontrol)

(import (only (com jme3 math FastMath) PI))

(import (class com.jme3.asset AssetManager))
(import (class com.jme3.effect ParticleEmitter ParticleMesh))
(import (class com.jme3.material Material))
(import (class com.jme3.material RenderState))
(import (class com.jme3.math ColorRGBA Quaternion Vector3f))
(import (class com.jme3.renderer.queue RenderQueue))
(import (class com.jme3.scene Geometry Node))
(import (class com.jme3.scene.shape Box Dome Cylinder Quad Sphere))
(import (class com.jme3.texture Texture))
(import (class java.lang Class))

(define (make-pyramid radius color::ColorRGBA glow-color::ColorRGBA)
  (let* ((asset-manager (get-asset-manager))
         (blendMode RenderState:BlendMode)
         (bucket RenderQueue:Bucket)
         (pyramid::Dome (Dome 2 4 radius))
         (pyramid-geom::Geometry (Geometry "Pyramid" pyramid))
         (pyramid-mat::Material (Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
         (pyramid-color (ColorRGBA 1.0 0.0 0.0 0.5))
         (pyramid-glow-color (ColorRGBA 0.5 0.5 0.5 0.5)))
    (*:setColor pyramid-mat "Color" color)
    (*:setColor pyramid-mat "GlowColor" glow-color)
    (*:setBlendMode (*:getAdditionalRenderState pyramid-mat) blendMode:Alpha)
    (*:setMaterial pyramid-geom pyramid-mat)
    (*:setQueueBucket pyramid-geom bucket:Transparent)
    pyramid-geom))

;;; an orbital city near Jupiter belonging to the Caretakers
(define (make-adlivun-cell)
  (let* ((pivot (Node "Adlivun Cell"))
         (asset-manager::AssetManager (get-asset-manager))
         (emitter::ParticleEmitter (ParticleEmitter "Smoke" ParticleMesh:Type:Triangle 32))
         (emitter-mat::Material (Material asset-manager "Common/MatDefs/Misc/Particle.j3md"))
         (emitter-start-color::ColorRGBA (ColorRGBA 0.8 0.3 0.3 0.4))
         (emitter-end-color::ColorRGBA (ColorRGBA 0.5 0.2 0.2 0.2))
         ;; pyramid0 -- the central enclosing pyramid
         (pyramid0-geom::Geometry (make-pyramid 256.0 (ColorRGBA 1.0 0.0 0.0 0.5)(ColorRGBA 0.5 0.5 0.5 0.5)))
         ;; pyramid00 -- the center of the center
         (pyramid00-geom::Geometry (make-pyramid 84.0 (ColorRGBA 1.0 0.5 0.0 0.5)(ColorRGBA 0.5 0.5 0.5 0.5)))
         (pyramid000-geom::Geometry (make-pyramid 7.0 (ColorRGBA 1.0 1.0 1.0 1.0)(ColorRGBA 1.0 1.0 1.0 1.0)))
         ;; pyramid01 -- the right-hand interior pyramid
         (pyramid01-geom::Geometry (make-pyramid 84.0 (ColorRGBA 1.0 0.5 0.0 0.5)(ColorRGBA 0.5 0.5 0.5 0.5)))
         (pyramid010-geom::Geometry (make-pyramid 7.0 (ColorRGBA 1.0 1.0 1.0 1.0)(ColorRGBA 1.0 1.0 1.0 1.0)))
         ;; pyramid02 -- the rear interior pyramid
         (pyramid02-geom::Geometry (make-pyramid 84.0 (ColorRGBA 1.0 0.5 0.0 0.5)(ColorRGBA 0.5 0.5 0.5 0.5)))
         (pyramid020-geom::Geometry (make-pyramid 7.0 (ColorRGBA 1.0 1.0 1.0 1.0)(ColorRGBA 1.0 1.0 1.0 1.0)))
         ;; pyramid03 -- the left interior pyramid
         (pyramid03-geom::Geometry (make-pyramid 84.0 (ColorRGBA 1.0 0.5 0.0 0.5)(ColorRGBA 0.5 0.5 0.5 0.5)))
         (pyramid030-geom::Geometry (make-pyramid 7.0 (ColorRGBA 1.0 1.0 1.0 1.0)(ColorRGBA 1.0 1.0 1.0 1.0)))
         ;; pyramid04 -- the front interior pyramid
         (pyramid04-geom::Geometry (make-pyramid 84.0 (ColorRGBA 1.0 0.5 0.0 0.5)(ColorRGBA 0.5 0.5 0.5 0.5)))
         (pyramid040-geom::Geometry (make-pyramid 7.0 (ColorRGBA 1.0 1.0 1.0 1.0)(ColorRGBA 1.0 1.0 1.0 1.0)))
         ;; pyramid05 -- the top interior pyramid
         (pyramid05-geom::Geometry (make-pyramid 84.0 (ColorRGBA 1.0 0.5 0.0 0.5)(ColorRGBA 0.5 0.5 0.5 0.5)))
         (pyramid050-geom::Geometry (make-pyramid 7.0 (ColorRGBA 1.0 1.0 1.0 1.0)(ColorRGBA 1.0 1.0 1.0 1.0)))
         ;; pyramid06 -- the bottom interior pyramid
         (pyramid06-geom::Geometry (make-pyramid 84.0 (ColorRGBA 1.0 0.5 0.0 0.5)(ColorRGBA 0.5 0.5 0.5 0.5)))
         (pyramid060-geom::Geometry (make-pyramid 7.0 (ColorRGBA 1.0 1.0 1.0 1.0)(ColorRGBA 1.0 1.0 1.0 1.0))))
    (*:setLocalTranslation pivot 0.0 0.0 0.0)
    
    (*:attachChild pivot pyramid0-geom)
    (*:setLocalTranslation pyramid0-geom 0.0 0.0 0.0)    

    (*:attachChild pivot pyramid00-geom)
    (*:setLocalTranslation pyramid00-geom 0.0 0.0 0.0)
    (*:attachChild pivot pyramid000-geom)
    (*:setLocalTranslation pyramid000-geom 0.0 0.0 0.0)

    (*:attachChild pivot pyramid01-geom)
    (*:setLocalTranslation pyramid01-geom 168.0 0.0 0.0)
    (*:attachChild pivot pyramid010-geom)
    (*:setLocalTranslation pyramid010-geom 168.0 0.0 0.0)
    
    (*:attachChild pivot pyramid02-geom)
    (*:setLocalTranslation pyramid02-geom 0.0 0.0 -168.0)
    (*:attachChild pivot pyramid020-geom)
    (*:setLocalTranslation pyramid020-geom 0.0 0.0 -168.0)
    
    (*:attachChild pivot pyramid03-geom)
    (*:setLocalTranslation pyramid03-geom -168.0 0.0 0.0)
    (*:attachChild pivot pyramid030-geom)
    (*:setLocalTranslation pyramid030-geom -168.0 0.0 0.0)
    
    (*:attachChild pivot pyramid04-geom)
    (*:setLocalTranslation pyramid04-geom 0.0 0.0 168.0)
    (*:attachChild pivot pyramid040-geom)
    (*:setLocalTranslation pyramid040-geom 0.0 0.0 168.0)
    
    (*:attachChild pivot pyramid05-geom)
    (*:setLocalTranslation pyramid05-geom 0.0 168.0 0.0)
    (*:attachChild pivot pyramid050-geom)
    (*:setLocalTranslation pyramid050-geom 0.0 168.0 0.0)
    
    (*:attachChild pivot pyramid06-geom)
    (*:setLocalTranslation pyramid06-geom 0.0 -168.0 0.0)
    (*:attachChild pivot pyramid060-geom)
    (*:setLocalTranslation pyramid060-geom 0.0 -168.0 0.0)

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
    (*:setLowLife emitter 32)
    (*:setHighLife emitter 128)
    (*:setVelocityVariation (*:getParticleInfluencer emitter) 2)
    (*:attachChild pivot emitter)

    pivot))

;;; an orbital city near Jupiter belonging to the Caretakers
(define (make-adlivun)
  (let* ((pivot (Node "Adlivun City"))
         (rotator::RotateControl (RotateControl 0 0 0.05))
         (cell0::Node (make-adlivun-cell))
         (cell1::Node (make-adlivun-cell))
         (cell2::Node (make-adlivun-cell))
         (cell3::Node (make-adlivun-cell))
         (cell4::Node (make-adlivun-cell))
         (cell5::Node (make-adlivun-cell))
         (cell6::Node (make-adlivun-cell)))
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
    (*:addControl pivot rotator)
    pivot))
