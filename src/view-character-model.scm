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
 blank-character-model
 make-character-model
 recolor-character-model!)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require util-color)
(require client)
(require data-assets)
(require model-rect)
(require model-character)
(require model-namegen)
(require view-armor-model)
(require view-augment-model)
(require view-character-model)
(require view-weapon-model)
(require view-rotatecontrol)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class gnu.mapping Symbol))
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

;;; (blank-character-model)
;;; ---------------------------------------------------------------------
;;; constructs an unnamed, undecorated character-model

(define (blank-character-model)
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


;;; (make-character-model character)
;;; ---------------------------------------------------------------------
;;; assembles a character model that reflects a character's name and
;;; attributes

(define (make-character-model character::FabricCharacter)
  (let* ((model (blank-character-model))
         (name::FabricName character:name)
         (faction::Symbol character:faction)
         (armor::Symbol character:armor)
         (augment::Symbol character:augment)
         (weapon::Symbol character:weapon)
         (lit-color::ColorRGBA (if (eqv? #!null faction)
                                   (default-glow-color)
                                   (faction-lit-color faction)))
         (dim-color::ColorRGBA (if (eqv? #!null faction)
                                   (default-character-color)
                                   (faction-dim-color faction)))
         (armor-model::Node (make-armor-model armor))
         (weapon-model::Node (make-weapon-model weapon))
         (augment-model::Node (make-augment-model augment)))
    (recolor-character-model! character model lit-color dim-color)
    (unless (eqv? #!null armor-model)
      (*:attachChild model armor-model))
    (unless (eqv? #!null weapon-model)
      (*:attachChild model weapon-model))
    (unless (eqv? #!null augment-model)
      (*:attachChild model augment-model))
    model))


