;;;; ***********************************************************************
;;;;
;;;; Name:          view-player-character.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       player-character models
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export abjurers-character-color caretakers-character-color
               make-player-character rogues-character-color
               set-player-character-cube-color!)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; 

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "model-entity.scm")
(require "syntax-classes.scm")

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
;;; player characters
;;; ---------------------------------------------------------------------

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (make-component-cube x y z)
  (let* ((asset-manager (get-asset-manager))
         (mat (Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
         (dim-color (ColorRGBA 0.25 0.25 0.25 0.125))
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

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (make-player-character-cube)
  (let ((i 0)
        (indexes '(-1.5 -0.5 0.5 1.5))
        (cubes '())
        (cubes-pivot (Node "Cubes pivot")))
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

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (make-player-character)
  (let* ((cube (make-player-character-cube)))
    (entity 'player-character cube: cube)))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (caretakers-character-color)
  (ColorRGBA 0.0 0.4 0.0 0.3))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (rogues-character-color)
  (ColorRGBA 0.0 0.3 0.5 0.3))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (abjurers-character-color)
  (ColorRGBA 0.4 0.0 0.0 0.3))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (set-player-character-cube-color! char-cube::Node character-color)
  (let* ((cubes::SafeArrayList (*:getChildren char-cube))
         (cube-count (*:size cubes)))
    (let loop ((i 0))
      (if (< i cube-count)
          (let* ((cube::Geometry (*:get cubes i))
                 (mat::Material (*:getMaterial cube)))
            (*:setColor mat "Color" character-color)
            (loop (+ i 1)))
          char-cube))))
