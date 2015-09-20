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
 character-model-namecubes
 character-model-root
 fabric-character-namestring
 make-fabric-character
 recolor-character-model!
 set-character-armor!
 set-character-augment!
 set-character-weapon!
 set-fabric-name!)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require util-error)
(require util-java)
(require util-lists)
(require model-namegen)
(require view-character-model)
(require view-armor-model)
(require view-augment-model)
(require view-weapon-model)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as ColorRGBA com.jme3.math.ColorRGBA)
(import-as Geometry com.jme3.scene.Geometry)
(import-as Material com.jme3.material.Material)
(import-as Node com.jme3.scene.Node)
(import-as SafeArrayList com.jme3.util.SafeArrayList)
(import-as Spatial com.jme3.scene.Spatial)
(import-as Symbol gnu.mapping.Symbol)

;;; ---------------------------------------------------------------------
;;; FabricCharacter
;;; ---------------------------------------------------------------------

(define-simple-class FabricCharacter ()
  (name::FabricName init: #!null)
  (model::Node init: #!null)
  (faction init-form: #!null)
  (weapon init-form: #!null)
  (weapon-model init-form: #!null)
  (armor init-form: #!null)
  (armor-model init-form: #!null)
  (augment init-form: #!null)
  (augment-model init-form: #!null))

(define (set-character-weapon! character::FabricCharacter weapon-name::Symbol)
  (let ((previous-model character:weapon-model)
        (new-model::Node (make-weapon-model weapon-name))
        (character-model::Node character:model))
    (set! character:weapon weapon-name)
    (set! character:weapon-model new-model)
    (unless (eqv? #!null previous-model)
      (*:detachChild character-model previous-model))
    (*:attachChild character-model new-model)))

(define (set-character-armor! character::FabricCharacter armor-name::Symbol)
  (let ((previous-model character:armor-model)
        (new-model::Node (make-armor-model armor-name))
        (character-model::Node character:model))
    (set! character:armor armor-name)
    (set! character:armor-model new-model)
    (unless (eqv? #!null previous-model)
        (*:detachChild character-model previous-model))
    (*:attachChild character-model new-model)))
  
(define (set-character-augment! character::FabricCharacter augment-name::Symbol)
  (let ((previous-model character:augment-model)
        (new-model::Node (make-augment-model augment-name))
        (character-model::Node character:model))
    (set! character:augment augment-name)
    (set! character:augment-model new-model)
    (unless (eqv? #!null previous-model)
      (*:detachChild character-model previous-model))
    (*:attachChild character-model new-model)))
  
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

(define (character-model-root character::FabricCharacter)
  character:model)

(define (character-model-namecubes character::FabricCharacter)
  (let* ((model::Node character:model)
         (namecube::Node (*:getChild model "NameCube")))
    (*:getChildren namecube)))

(define (recolor-character-model! character::FabricCharacter lit-color::ColorRGBA dim-color::ColorRGBA)
  (let* ((cubes::SafeArrayList (character-model-namecubes character))
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
