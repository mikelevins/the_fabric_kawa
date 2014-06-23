;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          name-cube.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       representation of a Fabric name as a cube of cubes
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; Fabric names are 64-bit values that have representations as numbers,
;;; as strings, and as 3D graphics. This file implements the representation
;;; of a Fabric name as a 4x4x4 cube of cubes. Each 1 bit in the Fabric
;;; name is represented as a brightly-lit cube; each 0 bit is represented as
;;; a dark, translucent cube

(module-export make-name-cube)


(require "util-java.scm")
(require "assets-general.scm")
(require "view-colors.scm")
(require "view-controls.scm")
(require "util-random.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(define-private-alias Box com.jme3.scene.shape.Box)
(define-private-alias ColorRGBA com.jme3.math.ColorRGBA)
(define-private-alias Geometry com.jme3.scene.Geometry)
(define-private-alias Material com.jme3.material.Material)
(define-private-alias Node com.jme3.scene.Node)
(define-private-alias RenderQueue com.jme3.renderer.queue.RenderQueue)
(define-private-alias RenderState com.jme3.material.RenderState)

;;; ---------------------------------------------------------------------
;;; building the name cubes
;;; ---------------------------------------------------------------------

(define name-dim-color (make-parameter (ColorRGBA 0.25 0.25 0.25 0.125)))

;;; make-name-cube
;;; ---------------------------------------------------------------------
;;; constructs the lit name cube from the pattern of bits

(define (make-component-cube lit? color activity x y z)
  (let* ((asset-manager (get-asset-manager))
         (mat (Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
         (dim-color color)
         (bright-color (brighten color))
         (flicker-color (ColorRGBA 0.9 0.9 1.0 0.6))
         (flickerer (<flicker-control>  dim-color flicker-color activity))
         (new-box (Box 0.4 0.4 0.4))
         (new-geom::Geometry (Geometry (format #f "cube ~a,~a, ~a" x y z) new-box))
         (bucket RenderQueue:Bucket))
    (let* ((blendMode RenderState:BlendMode))
      (@ 'setBlendMode (@ 'getAdditionalRenderState mat) blendMode:Alpha))
    (@ 'setMaterial new-geom mat)
    (@ 'setColor mat "Color" (if lit? color dim-color))
    (@ 'setColor mat "GlowColor" (if lit? bright-color dim-color))
    (@ 'setQueueBucket new-geom bucket:Transparent)
    (@ 'setLocalTranslation new-geom x y z)
    (unless lit?
      (@ 'addControl new-geom flickerer))
    new-geom))

(define (make-name-cube bits::gnu.math.IntNum lit-color::ColorRGBA)
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
             (let* ((lit? (bitwise-bit-set? bits i))
                    (new-geom (make-component-cube lit? lit-color (+ 24 (random-integer 128)) x y z)))
               
               (set! cubes (cons new-geom cubes))
               (set! i (+ i 1))))
           indexes))
        indexes))
     indexes)
    (@ 'setLocalTranslation cubes-pivot 0 0 0)
    (for-each (lambda (cube)(@ 'attachChild cubes-pivot cube))
              cubes)
    cubes-pivot))


