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
(require view-shapes)

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

;;; an orbital city near Jupiter belonging to the Caretakers
(define (make-adlivun-cell)
  (let* ((pivot (Node "Adlivun Cell"))
         (asset-manager::AssetManager (get-asset-manager))
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
    pivot))

;;; an orbital city near Sedna belonging to the Abjurers
(define (make-adlivun)
  (let* ((pivot (Node "Adlivun City"))
         (asset-manager::AssetManager (get-asset-manager))
         (emitter0::ParticleEmitter (ParticleEmitter "Smoke" ParticleMesh:Type:Triangle 32))
         (emitter1::ParticleEmitter (ParticleEmitter "Smoke" ParticleMesh:Type:Triangle 32))
         (mat::Material (Material asset-manager "Common/MatDefs/Misc/Particle.j3md"))
         (start-color::ColorRGBA (ColorRGBA 0.6 0.2 0.0 0.9))
         (end-color::ColorRGBA (ColorRGBA 0.3 0.1 0.0 0.1))
         (rotator::RotateControl (RotateControl 0 0.05 0))
         (cell0::Node (make-adlivun-cell))
         (cell1::Node (make-adlivun-cell))
         (cell2::Node (make-adlivun-cell))
         (cell3::Node (make-adlivun-cell))
         (cell4::Node (make-adlivun-cell))
         (cell6::Node (make-adlivun-cell))
         (cell7::Node (make-adlivun-cell))
         (obelisk0::Node (make-double-obelisk 16 1024 (ColorRGBA 0.25 0.0 0.0 0.8)(ColorRGBA 0.5 0.5 0.5 0.5)))
         (obelisk1::Node (make-double-obelisk 16 768 (ColorRGBA 0.25 0.0 0.0 0.8)(ColorRGBA 0.5 0.5 0.5 0.5))))

    (*:setTexture mat "Texture" (*:loadTexture asset-manager "Effects/Smoke/Smoke.png"))
    (*:setMaterial emitter0 mat)
    (*:setMaterial emitter1 mat)
    (*:setImagesX emitter0 15)
    (*:setImagesX emitter1 15)
    (*:setImagesY emitter0 1)
    (*:setImagesY emitter1 1)
    (*:setEndColor emitter0 end-color)
    (*:setEndColor emitter1 end-color)
    (*:setStartColor emitter0 start-color)
    (*:setStartColor emitter1 start-color)
    (*:setInitialVelocity (*:getParticleInfluencer emitter0)
                          (Vector3f 0.4 0.4 0.5))
    (*:setInitialVelocity (*:getParticleInfluencer emitter1)
                          (Vector3f 0.4 0.4 0.5))
    (*:setNumParticles emitter0 64)
    (*:setNumParticles emitter1 64)
    (*:setParticlesPerSec emitter0 64)
    (*:setParticlesPerSec emitter1 68)
    (*:setSelectRandomImage emitter0 #t)
    (*:setSelectRandomImage emitter1 #t)
    (*:setRandomAngle emitter0 #t)
    (*:setRandomAngle emitter1 #t)
    (*:setStartSize emitter0 8)
    (*:setStartSize emitter1 8)
    (*:setEndSize emitter0 256)
    (*:setEndSize emitter1 256)
    (*:setGravity emitter0 0 2 0)
    (*:setGravity emitter1 0 2 0)
    (*:setLowLife emitter0 8)
    (*:setLowLife emitter1 6)
    (*:setHighLife emitter0 64)
    (*:setHighLife emitter1 56)
    (*:setVelocityVariation (*:getParticleInfluencer emitter0) 1)
    (*:setVelocityVariation (*:getParticleInfluencer emitter1) 1.2)
    (*:attachChild obelisk0 emitter0)
    (*:attachChild obelisk1 emitter1)
    
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
    (*:attachChild pivot cell7)
    (*:setLocalTranslation cell6 1024.0 0.0 0.0)    
    (*:attachChild pivot obelisk0)
    (*:setLocalTranslation obelisk0 0.0 0.0 0.0)
    (*:attachChild pivot obelisk1)
    (*:setLocalTranslation obelisk1 1024.0 0.0 0.0)
    (*:addControl pivot rotator)
    pivot))
