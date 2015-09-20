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
 make-character-model)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require util-java)
(require util-error)
(require util-color)
(require client-class)
(require data-assets)
(require model-rect)
(require model-character)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as Box com.jme3.scene.shape.Box)
(import-as ColorRGBA com.jme3.math.ColorRGBA)
(import-as Geometry com.jme3.scene.Geometry)
(import-as Material com.jme3.material.Material)
(import-as Node com.jme3.scene.Node)
(import-as RenderQueue com.jme3.renderer.queue.RenderQueue)
(import-as RenderState com.jme3.material.RenderState)
(import-as SafeArrayList com.jme3.util.SafeArrayList)

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
        (model::Node (Node "CharacterModel")))
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
    (for-each (lambda (cube)(*:attachChild model cube))
              cubes)
    model))