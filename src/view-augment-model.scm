;;;; ***********************************************************************
;;;;
;;;; Name:          view-augment-model.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       3D augment models 
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 make-augment-model)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require util-error)
(require util-color)
(require client-class)
(require data-assets)
(require model-rect)
(require model-character)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (only (com jme3 math FastMath) PI))

(import (class com.jme3.asset AssetManager))
(import (class com.jme3.effect ParticleEmitter ParticleMesh))
(import (class com.jme3.material Material RenderState))
(import (class com.jme3.math ColorRGBA Quaternion Vector3f))
(import (class com.jme3.renderer.queue RenderQueue))
(import (class com.jme3.scene Geometry Node))
(import (class com.jme3.scene.debug WireBox WireSphere))
(import (class com.jme3.scene.shape Box Cylinder Dome PQTorus Sphere Torus))
(import (class com.jme3.util SafeArrayList))
(import (class gnu.mapping Symbol))

;;; ---------------------------------------------------------------------
;;; 
;;; ---------------------------------------------------------------------

;;; (make-augment-model)
;;; ---------------------------------------------------------------------

(define (make-augment-model augment-name::Symbol)
  (case augment-name
    ((force)(make-force-augment))
    ((optics)(make-optics-augment))
    ((portals)(make-portals-augment))
    ((turrets)(make-turrets-augment))
    (else #!null)))


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
         (optics-mat::Material (Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
         (color (ColorRGBA 0.3 0.2 0.1 0.5))
         (glow-color (ColorRGBA 0.9 0.9 1 0.9))
         (pivot (Node "CharacterWeapon"))
         (pivot0 (Node "ModulePivot0"))
         (pivot1 (Node "ModulePivot1")))
    (*:setColor optics-mat "Color" color)
    (*:setColor optics-mat "GlowColor" glow-color)
    (let* ((blendMode RenderState:BlendMode))
      (*:setBlendMode (*:getAdditionalRenderState optics-mat) blendMode:Alpha))
    (let* ((optics-module0 (Dome (Vector3f 0 0 0) 2 4 0.25 #f))
           (optics-module1 (Dome (Vector3f 0 0 0) 2 4 0.25 #f))
           (optics-module2 (Dome (Vector3f 0 0 0) 2 4 0.25 #f))
           (optics-module3 (Dome (Vector3f 0 0 0) 2 4 0.25 #f))
           (geom0::Geometry (Geometry "Module0" optics-module0))
           (geom1::Geometry (Geometry "Module1" optics-module1))
           (geom2::Geometry (Geometry "Module2" optics-module2))
           (geom3::Geometry (Geometry "Module3" optics-module3))
           (bucket RenderQueue:Bucket))
      (*:setMaterial geom0 optics-mat)
      (*:setMaterial geom1 optics-mat)
      (*:setMaterial geom2 optics-mat)
      (*:setMaterial geom3 optics-mat)
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
;;; portals augment
;;; ---------------------------------------------------------------------

(define (make-portals-augment)
  (let* ((asset-manager (get-asset-manager))
         (sphere-mat::Material (Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
         (torus-mat::Material (Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
         (sphere-color (ColorRGBA 0 0 0 0.5))
         (sphere-glow-color (ColorRGBA 1 1 1 1))
         (torus-color (ColorRGBA 0.5 0 0.7 0.25))
         (torus-glow-color (ColorRGBA 0.9 0.9 1 0.7))
         (pivot (Node "CharacterWeapon"))
         (pivot0 (Node "ModulePivot0"))
         (pivot1 (Node "ModulePivot1")))
    (*:setColor sphere-mat "Color" sphere-color)
    (*:setColor sphere-mat "GlowColor" sphere-glow-color)
    (*:setColor torus-mat "Color" torus-color)
    (*:setColor torus-mat "GlowColor" torus-glow-color)
    (let* ((blendMode RenderState:BlendMode))
      (*:setBlendMode (*:getAdditionalRenderState sphere-mat) blendMode:Alpha)
      (*:setBlendMode (*:getAdditionalRenderState torus-mat) blendMode:Alpha))
    (let* ((torus0::Torus (Torus 32 32 0.1 0.25))
           (sphere0::Sphere (Sphere 32 32 0.1))
           (torus1::Torus (Torus 32 32 0.1 0.25))
           (sphere1::Sphere (Sphere 32 32 0.1))
           (geom0::Geometry (Geometry "Module0" torus0))
           (geom1::Geometry (Geometry "Module1" sphere0))
           (geom2::Geometry (Geometry "Module2" torus1))
           (geom3::Geometry (Geometry "Module3" sphere1))
           (bucket RenderQueue:Bucket))
      (*:setMaterial geom0 torus-mat)
      (*:setMaterial geom1 sphere-mat)
      (*:setMaterial geom2 torus-mat)
      (*:setMaterial geom3 sphere-mat)
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
;;; turrets augment
;;; ---------------------------------------------------------------------

(define (make-turrets-augment)
  (let* ((asset-manager (get-asset-manager))
         (turrets-mat::Material (Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
         (color (ColorRGBA 0.6 0.3 0 0.7))
         (glow-color (ColorRGBA 0.9 0.8 0.7 0.9))
         (pivot (Node "CharacterWeapon"))
         (pivot0 (Node "ModulePivot0"))
         (pivot1 (Node "ModulePivot1")))
    (*:setColor turrets-mat "Color" color)
    (*:setColor turrets-mat "GlowColor" glow-color)
    (let* ((blendMode RenderState:BlendMode))
      (*:setBlendMode (*:getAdditionalRenderState turrets-mat) blendMode:Alpha))
    (let* ((turrets-module0 (WireBox 0.25 0.25 0.25))
           (turrets-module1 (WireBox 0.25 0.25 0.25))
           (turrets-module2 (WireBox 0.25 0.25 0.25))
           (turrets-module3 (WireBox 0.25 0.25 0.25))
           (geom0::Geometry (Geometry "Module0" turrets-module0))
           (geom1::Geometry (Geometry "Module1" turrets-module1))
           (geom2::Geometry (Geometry "Module2" turrets-module2))
           (geom3::Geometry (Geometry "Module3" turrets-module3))
           (bucket RenderQueue:Bucket))
      (*:setMaterial geom0 turrets-mat)
      (*:setMaterial geom1 turrets-mat)
      (*:setMaterial geom2 turrets-mat)
      (*:setMaterial geom3 turrets-mat)
      (*:setQueueBucket geom0 bucket:Transparent)
      (*:setQueueBucket geom1 bucket:Transparent)
      (*:setQueueBucket geom2 bucket:Transparent)
      (*:setQueueBucket geom3 bucket:Transparent)
      (*:setLocalTranslation geom0 0.0 0.0 0.0)
      (*:setLocalTranslation geom1 0.0 0.0 0.0)
      (*:setLocalTranslation geom2 0.0 0.0 0.0)
      (*:setLocalTranslation geom3 0.0 0.0 0.0)
      (let ((rotation (Quaternion))
            (pitch-axis0 (Vector3f 1 0 0))
            (pitch-axis1 (Vector3f 0 0 1)))
        (*:fromAngleAxis rotation (/ PI 4) pitch-axis0)
        (*:fromAngleAxis rotation (/ PI 4) pitch-axis1)
        (*:setLocalRotation geom1 rotation)
        (*:setLocalRotation geom3 rotation))
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

