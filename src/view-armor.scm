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
 make-absorb-armor)

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

(import-as ColorRGBA com.jme3.math.ColorRGBA)
(import-as Geometry com.jme3.scene.Geometry)
(import-as Material com.jme3.material.Material)
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
