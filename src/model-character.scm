;;;; ***********************************************************************
;;;;
;;;; Name:          model-character.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       The common character class
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 FabricCharacter
 fabric-character-namestring
 make-fabric-character
 recolor-character-model!
 set-fabric-name!)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require util-error)
(require util-java)
(require util-lists)
(require model-namegen)
(require view-character-model)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as ColorRGBA com.jme3.math.ColorRGBA)
(import-as Geometry com.jme3.scene.Geometry)
(import-as Material com.jme3.material.Material)
(import-as Node com.jme3.scene.Node)
(import-as SafeArrayList com.jme3.util.SafeArrayList)

;;; ---------------------------------------------------------------------
;;; FabricCharacter
;;; ---------------------------------------------------------------------

(define-simple-class FabricCharacter ()
  (name::FabricName init: #!null)
  (model::Node init: #!null)
  (faction init-form: #!null)
  )

(define (make-fabric-character fname::FabricName)
  (let* ((fchar (FabricCharacter))
         (fmodel (make-character-model fchar)))
    (set! fchar:model fmodel)
    (set! fchar:name fname)
    fchar))

(define (fabric-character-namestring fchar::FabricCharacter)
  (apply string-append (interpose " " (fabric-name-strings fchar:name))))

(define (set-fabric-name! fchar::FabricCharacter fname::FabricName)
  (set! fchar:name fname))

(define (recolor-character-model! character::FabricCharacter lit-color::ColorRGBA dim-color::ColorRGBA)
  (let* ((model::Node character:model)
         (cubes::SafeArrayList (*:getChildren model))
         (name::FabricName character:name)
         (name-bits (fabric-name-bits name))
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
