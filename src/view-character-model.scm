;;;; ***********************************************************************
;;;;
;;;; Name:          view-character-model.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       3D character models 
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 make-character-model
 recolor-character-model!)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require util-error)
(require util-color)
(require client)
(require data-assets)
(require model-rect)
(require model-character)
(require model-namegen)
(require view-character-model)
(require view-rotatecontrol)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class com.jme3.material Material RenderState))
(import (class com.jme3.math ColorRGBA))
(import (class com.jme3.renderer.queue RenderQueue))
(import (class com.jme3.scene Geometry Node))
(import (class com.jme3.scene.shape Box))
(import (class com.jme3.util SafeArrayList))

;;; ---------------------------------------------------------------------
;;; 
;;; ---------------------------------------------------------------------

;;; (make-component-cube x y z)
;;; ---------------------------------------------------------------------
;;; returns a new cube of the type used as a component in Fabric
;;; character avatars

(define (make-component-cube x y z)
  (let* ((asset-manager (get-asset-manager))
         (mat (Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
         (dim-color (default-character-color))
         (glow-color (default-glow-color))
         (new-box (Box 0.4 0.4 0.4))
         (new-geom::Geometry (Geometry (format #f "cube ~a,~a,~a" x y z) new-box))
         (bucket RenderQueue:Bucket))
    (let* ((blendMode RenderState:BlendMode))
      (*:setBlendMode (*:getAdditionalRenderState mat) blendMode:Alpha))
    (*:setMaterial new-geom mat)
    (*:setColor mat "Color" dim-color)
    (*:setColor mat "GlowColor" glow-color)
    (*:setQueueBucket new-geom bucket:Transparent)
    (*:setLocalTranslation new-geom x y z)
    new-geom))


;;; (make-character-model)
;;; ---------------------------------------------------------------------
;;; constructs the geometric node that serves as the image of a player
;;; or non-player character avatar in Fabric scenes. the node is
;;; constructed from 64 translucent, tinted cubes arranged in a cube.

(define (make-character-model character::FabricCharacter)
  (let ((i 0)
        (indexes '(-1.5 -0.5 0.5 1.5))
        (cubes '())
        (model::Node (Node "CharacterModel"))
        (namecube::Node (Node "NameCube"))
        (rotator::RotateControl (RotateControl 0.0 0.15 0.15)))
    (for-each
     (lambda (x)
       (for-each
        (lambda (y)
          (for-each
           (lambda (z)
             (let ((new-geom (make-component-cube x y z)))
               (set! cubes (cons new-geom cubes))
               (set! i (+ i 1))))
           indexes))
        indexes))
     indexes)
    (*:setLocalTranslation model 0 0 0)
    (*:setLocalTranslation namecube 0 0 0)
    (*:attachChild model namecube)
    (for-each (lambda (cube)(*:attachChild namecube cube))
              cubes)
    (*:addControl namecube rotator)
    model))

(define (recolor-character-model! character::FabricCharacter model::Node
                                  lit-color::ColorRGBA dim-color::ColorRGBA)
  (let* ((namecube::Node (*:getChild model "NameCube"))
         (cubes::SafeArrayList (*:getChildren namecube))
         (name::FabricName character:name)
         (name-bits (fabric-name->bits name))
         (glow-color (ColorRGBA 1 1 1 0.6))
         (cube-count (*:size cubes)))
    (let loop ((i 0))
      (if (< i cube-count)
          (let* ((cube::Geometry (*:get cubes i))
                 (mat::Material (*:getMaterial cube))
                 (flag (list-ref name-bits i)))
            (*:setColor mat "Color" (if flag lit-color dim-color))
            (*:setColor mat "GlowColor" (if flag glow-color dim-color))
            (loop (+ i 1)))))))
