;;;; ***********************************************************************
;;;;
;;;; Name:          view-shapes.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       building blocks for models
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 make-crystal
 make-double-obelisk
 make-obelisk
 make-pyramid)

(require data-assets)

(import (only (com jme3 math FastMath) PI))

(import (class com.jme3.material Material RenderState))
(import (class com.jme3.math ColorRGBA Quaternion Vector3f))
(import (class com.jme3.renderer.queue RenderQueue))
(import (class com.jme3.scene Geometry Node))
(import (class com.jme3.scene.shape Box Dome Cylinder Quad Sphere))

(define (make-pyramid radius color::ColorRGBA glow-color::ColorRGBA)
  (let* ((asset-manager (get-asset-manager))
         (blendMode RenderState:BlendMode)
         (bucket RenderQueue:Bucket)
         (pyramid::Dome (Dome (Vector3f 0 0 0) 2 4 radius #f))
         (pyramid-geom::Geometry (Geometry "Pyramid" pyramid))
         (pyramid-mat::Material (Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md")))
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
         (pyramid0::Dome (Dome (Vector3f 0 0 0) 2 4 radius #f))
         (pyramid1::Dome (Dome (Vector3f 0 0 0) 2 4 radius #f))
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

(define (make-obelisk width height color::ColorRGBA glow-color::ColorRGBA)
  (let* ((pivot::Node (Node "Obelisk"))
         (asset-manager (get-asset-manager))
         (blendMode RenderState:BlendMode)
         (bucket RenderQueue:Bucket)
         (pyramid-top::Geometry (make-pyramid (/ (* 23 width) 16) color glow-color))
         (box::Box (Box width height width))
         (box-geom::Geometry (Geometry "Obelisk Body" box))
         (obelisk-mat::Material (Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
         (rotation::Quaternion (Quaternion))
         (pitch-axis (Vector3f 1 0 0))
         (yaw-axis (Vector3f 0 1 0)))
    (*:setColor obelisk-mat "Color" color)
    (*:setColor obelisk-mat "GlowColor" glow-color)
    (*:setBlendMode (*:getAdditionalRenderState obelisk-mat) blendMode:Alpha)
    (*:setMaterial pyramid-top obelisk-mat)
    (*:setMaterial box-geom obelisk-mat)
    (*:setQueueBucket pyramid-top bucket:Transparent)
    (*:setQueueBucket box-geom bucket:Transparent)
    (*:attachChild pivot box-geom)
    (*:setLocalTranslation box-geom 0.0 0.0 0.0)
    (*:attachChild pivot pyramid-top)
    (*:fromAngleAxis rotation (/ PI 4.0) yaw-axis)
    (*:setLocalRotation pyramid-top rotation)
    (*:setLocalTranslation pyramid-top 0.0 height 0.0)
    pivot))

(define (make-double-obelisk width height color::ColorRGBA glow-color::ColorRGBA)
  (let* ((pivot::Node (Node "Obelisk"))
         (pitch-axis (Vector3f 1 0 0))
         (rotation::Quaternion (Quaternion))
         (obelisk-top (make-obelisk width (/ height 2.0) color glow-color))
         (obelisk-bottom (make-obelisk width (/ height 2.0) color glow-color)))
    (*:attachChild pivot obelisk-top)
    (*:setLocalTranslation obelisk-top 0.0 (/ height 2.0) 0.0)
    (*:attachChild pivot obelisk-bottom)
    (*:setLocalTranslation obelisk-bottom 0.0 (* -1 (/ height 2.0)) 0.0)
    (*:fromAngleAxis rotation PI pitch-axis)
    (*:setLocalRotation obelisk-bottom rotation)
    pivot))
