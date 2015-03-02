;;;; ***********************************************************************
;;;;
;;;; Name:          view-player-character.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       player-character models
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 abjurers-character-color
 caretakers-character-color
 default-character-color
 make-player-character
 rogues-character-color
 update-character-model!)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; this file provides code for constructing and initializing the
;;; geometric nodes used to represent player and non-player characters
;;; in Fabric scenes

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require 'list-lib)
(require "util-java.scm")
(require "util-lists.scm")
(require "model-entity.scm")
(require "data-names.scm")
(require "model-namegen.scm")
(require "syntax-classes.scm")
(require "view-colors.scm")
(require "data-assets.scm")
(require "appstate-character-creator.scm")

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
;;; characters
;;; ---------------------------------------------------------------------


;;; (make-component-cube x y z)
;;; ---------------------------------------------------------------------
;;; returns a new cube of the type used as a component in Fabric
;;; character avatars

(define (make-component-cube x y z)
  (let* ((asset-manager (get-asset-manager))
         (mat (Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
         (dim-color (default-character-color))
         (new-box (Box 0.4 0.4 0.4))
         (new-geom::Geometry (Geometry (format #f "cube ~a,~a,~a" x y z) new-box))
         (bucket RenderQueue:Bucket))
    (let* ((blendMode RenderState:BlendMode))
      (*:setBlendMode (*:getAdditionalRenderState mat) blendMode:Alpha))
    (*:setMaterial new-geom mat)
    (*:setColor mat "Color" dim-color)
    (*:setQueueBucket new-geom bucket:Transparent)
    (*:setLocalTranslation new-geom x y z)
    new-geom))


;;; (make-player-character-cube)
;;; ---------------------------------------------------------------------
;;; constructs the geometric node that serves as the image of a player
;;; or non-player character avatar in Fabric scenes. the node is
;;; constructed from 64 translucent, tinted cubes arranged in a cube.

(define (make-player-character-cube)
  (let ((i 0)
        (indexes '(-1.5 -0.5 0.5 1.5))
        (cubes '())
        (cubes-pivot (Node "CubesPivot")))
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
    (*:setLocalTranslation cubes-pivot 0 0 0)
    (for-each (lambda (cube)(*:attachChild cubes-pivot cube))
              cubes)
    cubes-pivot))


;;; (make-player-character)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed player-character entity that contains
;;; a cube node suitable for use as its avatar

(define (make-player-character)
  (let* ((character-node (Node "CharacterNode"))
         (cube (make-player-character-cube)))
    (*:attachChild character-node cube)
    (entity 'player-character node: character-node)))


(define (default-character-color)
  (ColorRGBA 0.25 0.25 0.25 0.25))


;;; (caretakers-character-color)
;;; ---------------------------------------------------------------------
;;; returns the standard color used to tint Caretaker characters

(define (caretakers-character-color)
  (ColorRGBA 0.0 0.4 0.0 0.3))


;;; (rogues-character-color)
;;; ---------------------------------------------------------------------
;;; returns the standard color used to tint Rogue characters

(define (rogues-character-color)
  (ColorRGBA 0.0 0.3 0.6 0.3))


;;; (abjurers-character-color)
;;; ---------------------------------------------------------------------
;;; returns the standard color used to tint Abjurer characters

(define (abjurers-character-color)
  (ColorRGBA 0.4 0.0 0.0 0.3))


;;; (character-glow-color)
;;; ---------------------------------------------------------------------
;;; returns the standard color used to make characters glow

(define (character-glow-color)
  (ColorRGBA 1.0 1.0 1.0 0.6))

;;; (get-faction-color)
;;; ---------------------------------------------------------------------
;;; returns the color associated with a specified faction

(define (get-faction-color faction)
  (case faction
    ((caretakers)(caretakers-character-color))
    ((rogues)(rogues-character-color))
    ((abjurers)(abjurers-character-color))
    (else (default-character-color))))

;;; (update-character-model! app-state::CharacterCreatorAppState)
;;; ---------------------------------------------------------------------
;;; sets the colors of all component cubes in a character's avatar cube

(define (update-character-model! app-state::CharacterCreatorAppState)
  (let* ((faction (*:getCurrentFaction app-state))
         (faction-color (get-faction-color faction))
         (bright-color (brighten faction-color))
         (dark-color (darken faction-color))
         (glow-color (character-glow-color))
         (character-name (*:getCharacterName app-state))
         (name-bits (fabric-name-bits character-name))
         (character (*:getCurrentCharacter app-state))
         (node::Node (get-property character 'node:))
         (pivot::Node (*:getChild node "CubesPivot"))
         (cubes::SafeArrayList (*:getChildren pivot))
         (cube-count (*:size cubes)))
    (let loop ((i 0))
      (if (< i cube-count)
          (let* ((cube::Geometry (*:get cubes i))
                 (mat::Material (*:getMaterial cube))
                 (flag (list-ref name-bits i)))
            (*:setColor mat "Color" (if flag bright-color dark-color))
            (*:setColor mat "GlowColor" (if flag glow-color dark-color))
            (loop (+ i 1)))))))
