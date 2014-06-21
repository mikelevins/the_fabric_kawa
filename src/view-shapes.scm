;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          shapes.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       tools for building 3D shapes
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export vertex make-vertex vertex->coordinates
               triangle triangle->vertexes
               make-enclosing-cube make-enclosing-wire-cube make-enclosing-sphere
               make-enclosing-wire-sphere make-enclosing-pyramid)

(require "util-java.scm")
(require "util-lists.scm")
(require "util-general.scm")
(require "assets-general.scm")
(require "view-controls.scm")
(require "view-colors.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(define-private-alias Box com.jme3.scene.shape.Box)
(define-private-alias Dome com.jme3.scene.shape.Dome)
(define-private-alias Geometry com.jme3.scene.Geometry)
(define-private-alias Material com.jme3.material.Material)
(define-private-alias RenderQueue com.jme3.renderer.queue.RenderQueue)
(define-private-alias RenderState com.jme3.material.RenderState)
(define-private-alias Sphere com.jme3.scene.shape.Sphere)
(define-private-alias Triangle com.jme3.math.Triangle)
(define-private-alias Vector3f com.jme3.math.Vector3f)
(define-private-alias WireBox com.jme3.scene.debug.WireBox)
(define-private-alias WireSphere com.jme3.scene.debug.WireSphere)

;;; ---------------------------------------------------------------------
;;; geometry constructors and deconstructors
;;; ---------------------------------------------------------------------

;;; coordinates -> vertex

(define (vertex x y z)
  (Vector3f x y z))

(define (make-vertex coordinates)
  (apply Vector3f coordinates))

;;; vertex -> coordinates

(define (vertex->coordinates v::Vector3f)
  (let* ((x (@ 'getX v))
         (y (@ 'getY v))
         (z (@ 'getZ v)))
    (list x y z)))

;;; vertexes -> triangles

(define (triangle v1::Vector3f v2::Vector3f v3::Vector3f)
  (Triangle v1 v2 v3))

;;; triangle -> vertexes

(define (triangle->vertexes t::Triangle)
  (let* ((v1 (@ 'get1 t))
         (v2 (@ 'get2 t))
         (v3 (@ 'get3 t)))
    (list v1 v2 v3)))

;;; triangles -> polygons



;;; polygon -> triangles



;;; ---------------------------------------------------------------------
;;; finished shape constructors
;;; ---------------------------------------------------------------------

(define (make-enclosing-cube)
  (let* ((asset-manager (get-asset-manager))
         (cube-mat::Material (Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
         (color (any-lit-color)))
    (@ 'setColor cube-mat "Color" color)
    (@ 'setColor cube-mat "GlowColor" (any-glow color))
    (let* ((blendMode RenderState:BlendMode))
      (@ 'setBlendMode (@ 'getAdditionalRenderState cube-mat) blendMode:Alpha))
    (let* ((r (choose-any '(3.25 3.5 3.75 4.0 4.25 4.5 4.75)))
           (new-box::Box (Box r r r))
           (new-geom::Geometry (Geometry (format #f "cubic armor") new-box))
           (rotator (any-rotator))
           (bucket RenderQueue:Bucket))
      (@ 'setMaterial new-geom cube-mat)
      (@ 'setQueueBucket new-geom bucket:Transparent)
      (@ 'addControl new-geom rotator)
      (@ 'setLocalTranslation new-geom 0 0 0)
      new-geom)))


(define (make-enclosing-wire-cube)
  (let* ((asset-manager (get-asset-manager))
         (cube-mat::Material (Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
         (color (any-lit-color)))
    (@ 'setColor cube-mat "Color" color)
    (@ 'setColor cube-mat "GlowColor" (any-glow color))
    (let* ((blendMode RenderState:BlendMode))
      (@ 'setBlendMode (@ 'getAdditionalRenderState cube-mat) blendMode:Alpha))
    (let* ((r (choose-any '(3.25 3.5 3.75 4.0 4.25 4.5 4.75)))
           (new-box::WireBox (WireBox r r r))
           (new-geom::Geometry (Geometry (format #f "cubic armor") new-box))
           (rotator (any-rotator))
           (bucket RenderQueue:Bucket))
      (@ 'setMaterial new-geom cube-mat)
      (@ 'setQueueBucket new-geom bucket:Transparent)
      (@ 'addControl new-geom rotator)
      (@ 'setLocalTranslation new-geom 0 0 0)
      new-geom)))


(define (make-enclosing-sphere)
  (let* ((asset-manager (get-asset-manager))
         (sphere-mat::Material (Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
         (color (any-lit-color)))
    (@ 'setColor sphere-mat "Color" color)
    (@ 'setColor sphere-mat "GlowColor" (any-glow color))
    (let* ((blendMode RenderState:BlendMode))
      (@ 'setBlendMode (@ 'getAdditionalRenderState sphere-mat) blendMode:Alpha))
    (let* ((r (choose-any '(3.5 3.75 4.0 4.25 4.5 4.75 5.0)))
           (new-sphere::Sphere (Sphere 32 32 r))
           (new-geom::Geometry (Geometry (format #f "spherical armor") new-sphere))
           (rotator (any-rotator))
           (bucket RenderQueue:Bucket))
      (@ 'setMaterial new-geom sphere-mat)
      (@ 'setQueueBucket new-geom bucket:Transparent)
      (@ 'addControl new-geom rotator)
      (@ 'setLocalTranslation new-geom 0 0 0)
      new-geom)))

(define (make-enclosing-wire-sphere)
  (let* ((asset-manager (get-asset-manager))
         (sphere-mat::Material (Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
         (color (any-lit-color)))
    (@ 'setColor sphere-mat "Color" color)
    (@ 'setColor sphere-mat "GlowColor" (any-glow color))
    (let* ((blendMode RenderState:BlendMode))
      (@ 'setBlendMode (@ 'getAdditionalRenderState sphere-mat) blendMode:Alpha))
    (let* ((r (choose-any '(3.75 4.0 4.25 4.5)))
           (new-sphere::WireSphere (WireSphere r))
           (new-geom::Geometry (Geometry (format #f "spherical armor") new-sphere))
           (rotator (any-rotator))
           (bucket RenderQueue:Bucket))
      (@ 'setMaterial new-geom sphere-mat)
      (@ 'setQueueBucket new-geom bucket:Transparent)
      (@ 'addControl new-geom rotator)
      (@ 'setLocalTranslation new-geom 0 0 0)
      new-geom)))

(define (make-enclosing-pyramid)
  (let* ((asset-manager (get-asset-manager))
         (shape-mat (Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
         (color (any-lit-color)))
    (@ 'setColor shape-mat "Color" color)
    (@ 'setColor shape-mat "GlowColor" (any-glow color))
    (let* ((blendMode RenderState:BlendMode))
      (@ 'setBlendMode (@ 'getAdditionalRenderState shape-mat) blendMode:Alpha))
    (let* ((new-shape (Dome 2 4 (choose-any '(5.75 6.0 6.25 6.5))))
           (new-geom (Geometry (format #f "pyramid armor") new-shape))
           (rotator (any-rotator))
           (bucket RenderQueue:Bucket))
      (@ 'setMaterial new-geom shape-mat)
      (@ 'setQueueBucket new-geom bucket:Transparent)
      (@ 'addControl new-geom rotator)
      (@ 'setLocalTranslation new-geom 0 0 0)
      new-geom)))




