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
 make-adlivun
 make-pyramid)

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


(define (make-crystal radius color::ColorRGBA glow-color::ColorRGBA)
  (let* ((asset-manager (get-asset-manager))
         (pivot (Node "Pyramid"))
         (blendMode RenderState:BlendMode)
         (bucket RenderQueue:Bucket)
         (pyramid0::Dome (Dome 2 4 radius))
         (pyramid1::Dome (Dome 2 4 radius))
         (pyramid0-geom::Geometry (Geometry "Pyramid0" pyramid0))
         (pyramid1-geom::Geometry (Geometry "Pyramid1" pyramid1))
         (rotation::Quaternion (Quaternion))
         (pitch-axis (Vector3f 1 0 0))
         (pyramid-mat::Material (Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
         (pyramid-color (ColorRGBA 1.0 0.0 0.0 0.5))
         (pyramid-glow-color (ColorRGBA 0.5 0.5 0.5 0.5)))
    (*:setColor pyramid-mat "Color" color)
    (*:setColor pyramid-mat "GlowColor" glow-color)
    (*:setBlendMode (*:getAdditionalRenderState pyramid-mat) blendMode:Alpha)
    (*:setMaterial pyramid0-geom pyramid-mat)
    (*:setMaterial pyramid1-geom pyramid-mat)
    (*:setQueueBucket pyramid0-geom bucket:Transparent)
    (*:setQueueBucket pyramid1-geom bucket:Transparent)
    (*:attachChild pivot pyramid0-geom)
    (*:setLocalTranslation pyramid0-geom 0.0 0.0 0.0)    
    (*:attachChild pivot pyramid1-geom)
    (*:setLocalTranslation pyramid1-geom 0.0 0.0 0.0)
    (*:fromAngleAxis rotation PI pitch-axis)
    (*:setLocalRotation pyramid1-geom rotation)
    pivot))

;;; an orbital city near Jupiter belonging to the Caretakers
(define (make-adlivun-cell)
  (let* ((pivot (Node "Adlivun Cell"))
         (asset-manager::AssetManager (get-asset-manager))
         (emitter::ParticleEmitter (ParticleEmitter "Smoke" ParticleMesh:Type:Triangle 32))
         (emitter-mat::Material (Material asset-manager "Common/MatDefs/Misc/Particle.j3md"))
         (emitter-start-color::ColorRGBA (ColorRGBA 0.6 0.2 0.1 0.4))
         (emitter-end-color::ColorRGBA (ColorRGBA 0.3 0.0 0.0 0.2))
         ;; crystal0 -- the central enclosing crystal
         (crystal0-geom::Node (make-crystal 256.0 (ColorRGBA 1.0 0.0 0.0 0.5)(ColorRGBA 0.5 0.5 0.5 0.5)))
         ;; crystal00 -- the center of the center
         (crystal00-geom::Node (make-crystal 84.0 (ColorRGBA 0.5 0.0 0.0 0.5)(ColorRGBA 0.5 0.5 0.5 0.5)))
         (crystal000-geom::Node (make-crystal 7.0 (ColorRGBA 0.8 0.5 0.5 0.5)(ColorRGBA 0.8 0.5 0.5 0.5)))
         ;; crystal01 -- the right-hand interior crystal
         (crystal01-geom::Node (make-crystal 84.0 (ColorRGBA 0.5 0.0 0.0 0.5)(ColorRGBA 0.5 0.5 0.5 0.5)))
         (crystal010-geom::Node (make-crystal 7.0 (ColorRGBA 0.8 0.5 0.5 0.5)(ColorRGBA 0.8 0.5 0.5 0.5)))
         ;; crystal02 -- the rear interior crystal
         (crystal02-geom::Node (make-crystal 84.0 (ColorRGBA 0.5 0.0 0.0 0.5)(ColorRGBA 0.5 0.5 0.5 0.5)))
         (crystal020-geom::Node (make-crystal 7.0 (ColorRGBA 0.8 0.5 0.5 0.5)(ColorRGBA 0.8 0.5 0.5 0.5)))
         ;; crystal03 -- the left interior crystal
         (crystal03-geom::Node (make-crystal 84.0 (ColorRGBA 0.5 0.0 0.0 0.5)(ColorRGBA 0.5 0.5 0.5 0.5)))
         (crystal030-geom::Node (make-crystal 7.0 (ColorRGBA 0.8 0.5 0.5 0.5)(ColorRGBA 0.8 0.5 0.5 0.5)))
         ;; crystal04 -- the front interior crystal
         (crystal04-geom::Node (make-crystal 84.0 (ColorRGBA 0.5 0.0 0.0 0.5)(ColorRGBA 0.5 0.5 0.5 0.5)))
         (crystal040-geom::Node (make-crystal 7.0 (ColorRGBA 0.8 0.5 0.5 0.5)(ColorRGBA 0.8 0.5 0.5 0.5)))
         ;; crystal05 -- the top interior crystal
         (crystal05-geom::Node (make-crystal 84.0 (ColorRGBA 0.5 0.0 0.0 0.5)(ColorRGBA 0.5 0.5 0.5 0.5)))
         (crystal050-geom::Node (make-crystal 7.0 (ColorRGBA 0.8 0.5 0.5 0.5)(ColorRGBA 0.8 0.5 0.5 0.5)))
         ;; crystal06 -- the bottom interior crystal
         (crystal06-geom::Node (make-crystal 84.0 (ColorRGBA 0.5 0.0 0.0 0.5)(ColorRGBA 0.5 0.5 0.5 0.5)))
         (crystal060-geom::Node (make-crystal 7.0 (ColorRGBA 0.8 0.5 0.5 0.5)(ColorRGBA 0.8 0.5 0.5 0.5))))
    (*:setLocalTranslation pivot 0.0 0.0 0.0)
    
    (*:attachChild pivot crystal0-geom)
    (*:setLocalTranslation crystal0-geom 0.0 0.0 0.0)    

    (*:attachChild pivot crystal00-geom)
    (*:setLocalTranslation crystal00-geom 0.0 0.0 0.0)
    (*:attachChild pivot crystal000-geom)
    (*:setLocalTranslation crystal000-geom 0.0 0.0 0.0)

    (*:attachChild pivot crystal01-geom)
    (*:setLocalTranslation crystal01-geom 168.0 0.0 0.0)
    (*:attachChild pivot crystal010-geom)
    (*:setLocalTranslation crystal010-geom 168.0 0.0 0.0)
    
    (*:attachChild pivot crystal02-geom)
    (*:setLocalTranslation crystal02-geom 0.0 0.0 -168.0)
    (*:attachChild pivot crystal020-geom)
    (*:setLocalTranslation crystal020-geom 0.0 0.0 -168.0)
    
    (*:attachChild pivot crystal03-geom)
    (*:setLocalTranslation crystal03-geom -168.0 0.0 0.0)
    (*:attachChild pivot crystal030-geom)
    (*:setLocalTranslation crystal030-geom -168.0 0.0 0.0)
    
    (*:attachChild pivot crystal04-geom)
    (*:setLocalTranslation crystal04-geom 0.0 0.0 168.0)
    (*:attachChild pivot crystal040-geom)
    (*:setLocalTranslation crystal040-geom 0.0 0.0 168.0)
    
    (*:attachChild pivot crystal05-geom)
    (*:setLocalTranslation crystal05-geom 0.0 168.0 0.0)
    (*:attachChild pivot crystal050-geom)
    (*:setLocalTranslation crystal050-geom 0.0 168.0 0.0)
    
    (*:attachChild pivot crystal06-geom)
    (*:setLocalTranslation crystal06-geom 0.0 -168.0 0.0)
    (*:attachChild pivot crystal060-geom)
    (*:setLocalTranslation crystal060-geom 0.0 -168.0 0.0)

    ;; the whole assembly
    (*:setTexture emitter-mat "Texture" (*:loadTexture asset-manager "Effects/Smoke/Smoke.png"))
    (*:setMaterial emitter emitter-mat)
    (*:setImagesX emitter 15)
    (*:setImagesY emitter 1)
    (*:setEndColor emitter emitter-end-color)
    (*:setStartColor emitter emitter-start-color)
    (*:setInitialVelocity (*:getParticleInfluencer emitter)
                          (Vector3f 0 6 3))
    (*:setNumParticles emitter 64)
    (*:setParticlesPerSec emitter 64)
    (*:setSelectRandomImage emitter #t)
    (*:setRandomAngle emitter #t)
    (*:setStartSize emitter 16)
    (*:setEndSize emitter 256)
    (*:setGravity emitter 0 2 0)
    (*:setLowLife emitter 16)
    (*:setHighLife emitter 32)
    (*:setVelocityVariation (*:getParticleInfluencer emitter) 2)
    (*:attachChild pivot emitter)

    pivot))

;;; an orbital city near Sedna belonging to the Abjurers
(define (make-adlivun)
  (let* ((pivot (Node "Adlivun City"))
         (rotator::RotateControl (RotateControl 0 0.03 0))
         (cell0::Node (make-adlivun-cell))
         (cell1::Node (make-adlivun-cell))
         (cell2::Node (make-adlivun-cell))
         (cell3::Node (make-adlivun-cell))
         (cell4::Node (make-adlivun-cell))
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
    (*:attachChild pivot cell6)
    (*:setLocalTranslation cell6 0.0 -512.0 0.0)    
    (*:addControl pivot rotator)
    pivot))
