;;;; ***********************************************************************
;;;;
;;;; Name:          view-location.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       construct supported locations
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 make-callisto
 make-dione
 make-earth
 make-enceladus
 make-europa
 make-ganymede
 make-iapetus
 make-io
 make-jupiter
 make-location
 make-mars
 make-mercury
 make-neptune
 make-pluto
 make-rhea
 make-saturn
 make-sedna
 make-tethys
 make-the-moon
 make-the-sun
 make-titan
 make-uranus
 make-venus)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require data-assets)
(require view-rotatecontrol)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (only (com jme3 math FastMath) PI))

(import (class com.jme3.asset AssetManager))
(import (class com.jme3.material Material))
(import (class com.jme3.material RenderState))
(import (class com.jme3.math ColorRGBA Quaternion Vector3f))
(import (class com.jme3.renderer.queue RenderQueue))
(import (class com.jme3.scene Geometry Node))
(import (class com.jme3.scene.shape Box Cylinder Quad Sphere))
(import (class com.jme3.texture Texture))
(import (class java.lang Class))

;;; ---------------------------------------------------------------------
;;; helpers
;;; ---------------------------------------------------------------------

(define (make-celestial-body texture-name radius rotation-rate)
  (let* ((texture-name (format #f "~A.jpg" texture-name))
         (asset-manager::AssetManager (get-asset-manager))
         (celestial-body::Sphere (Sphere 128 128 radius))
         (body-mat::Material (Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
         (body-texture (*:loadTexture asset-manager (string-append "Textures/" texture-name)))
         (body-pivot::Geometry (Geometry texture-name celestial-body))
         (body-rotator::RotateControl (RotateControl 0.0 0.0 rotation-rate))
         (rotation (Quaternion))
         (TextureMode Sphere:TextureMode)
         (Projected TextureMode:Projected)
         (pitch-axis (Vector3f 1 0 0)))
    (*:fromAngleAxis rotation (* -1 (/ PI 2)) pitch-axis)
    (*:setLocalRotation body-pivot rotation)
    (*:setLocalTranslation body-pivot 0 0 0)
    (*:setTextureMode celestial-body Projected)
    (*:setTexture body-mat "ColorMap" body-texture)
    (*:setMaterial body-pivot body-mat)
    (*:addControl body-pivot body-rotator)
    body-pivot))

(define (make-planetary-ring name texture-name radius rotation-rate #!key (bottom #f))
  (let* ((asset-manager::AssetManager (get-asset-manager))
         (rings::Quad (Quad (* 2 radius)(* 2 radius)))
         (rings-mat::Material (Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
         (rings-texture (*:loadTexture asset-manager (format #f "Textures/~A" texture-name)))
         (bucket RenderQueue:Bucket)
         (blendMode RenderState:BlendMode)
         (rings-pivot::Geometry (Geometry name rings))
         (rotation (Quaternion))
         (pitch-axis (Vector3f 1 0 0))
         (rotation-radians (if bottom
                               (* 1 (/ PI 2))
                               (* -1 (/ PI 2)))))
    (*:fromAngleAxis rotation rotation-radians pitch-axis)
    (*:setLocalRotation rings-pivot rotation)
    (*:setLocalTranslation rings-pivot
                           (* -1 radius)
                           0.0
                           (if bottom (* -1 radius) radius))
    (*:setTexture rings-mat "ColorMap" rings-texture)
    (*:setBlendMode (*:getAdditionalRenderState rings-mat) blendMode:Alpha)
    (*:setQueueBucket rings-pivot bucket:Transparent)
    (*:setMaterial rings-pivot rings-mat)
    rings-pivot))

;;; ---------------------------------------------------------------------
;;; landscape elements
;;; ---------------------------------------------------------------------

;;; an orbital city near Jupiter belonging to the Caretakers
(define (make-volvox)
  (let* ((pivot (Node "Volvox City"))
         (sphere::Sphere (Sphere 32 32 128))
         (sphere-geom::Geometry (Geometry "Sphere" sphere))
         (bucket RenderQueue:Bucket)
         (asset-manager (get-asset-manager))
         (sphere-mat::Material (Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
         (sphere-color (ColorRGBA 0.0 1.0 0.0 0.5))
         (sphere-glow-color (ColorRGBA 1 1 1 0.5))
         (blendMode RenderState:BlendMode))
    (*:setColor sphere-mat "Color" sphere-color)
    (*:setColor sphere-mat "GlowColor" sphere-glow-color)
    (*:setBlendMode (*:getAdditionalRenderState sphere-mat) blendMode:Alpha)
    (*:setMaterial sphere-geom sphere-mat)
    (*:setQueueBucket sphere-geom bucket:Transparent)
    (*:setLocalTranslation sphere-geom 0.0 0.0 0.0)
    (*:setLocalTranslation pivot 0.0 0.0 0.0)
    (*:attachChild pivot sphere-geom)
    pivot))

;;; ---------------------------------------------------------------------
;;; the locations
;;; ---------------------------------------------------------------------

(define (make-callisto)
  (make-celestial-body "Callisto" 1024 0.025))

(define (make-dione)
  (make-celestial-body "Dione" 1024 0.025))

(define (make-earth)
  (make-celestial-body "Earth" 2048 0.025))

(define (make-enceladus)
  (make-celestial-body "Enceladus" 1024 0.025))

(define (make-europa)
  (make-celestial-body "Europa" 1024 0.025))

(define (make-ganymede)
  (make-celestial-body "Ganymede" 1024 0.025))

(define (make-iapetus)
  (make-celestial-body "Iapetus" 1024 0.025))

(define (make-io)
  (make-celestial-body "Io" 1024 0.025))

(define (make-jupiter)
  (let* ((jupiter (make-celestial-body "Jupiter" 4096 0.05))
         (jupiter-pivot::Node (Node "Jupiter"))
         (volvox (make-volvox))
         (volvox-centroid (Node "Volvox Centroid"))
         (volvox-rotator (RotateControl 0.0 0.0 0.15))
         (volvox-centroid-rotator (RotateControl 0.0 0.0 0.015)))
    (*:setLocalTranslation volvox-centroid 0.0 0.0 0.0)
    (*:setLocalTranslation volvox 8000 0.0 0.0)
    (*:addControl volvox-centroid volvox-centroid-rotator)
    (*:addControl volvox volvox-rotator)
    (*:attachChild jupiter-pivot jupiter)
    (*:attachChild jupiter-pivot volvox-centroid)
    (*:attachChild volvox-centroid volvox)
    jupiter-pivot))

(define (make-mars)
  (make-celestial-body "Mars" 2048 0.025))

(define (make-mercury)
  (make-celestial-body "Mercury" 1024 0.025))

(define (make-neptune)
  (let* ((body::Geometry (make-celestial-body "Neptune" 3072 0.08))
         (ring-top::Geometry (make-planetary-ring "Neptune's Ring Top"
                                                  "neptune_ring_alpha.png" 12288 0.05))
         (ring-bottom::Geometry (make-planetary-ring "Neptune's Ring Bottom"
                                                     "neptune_ring_alpha.png" 12288 0.05 bottom: #t))
         (pivot::Node (Node "Neptune")))
    (*:attachChild pivot body)
    (*:attachChild pivot ring-top)
    (*:attachChild pivot ring-bottom)
    pivot))

(define (make-pluto)
  (make-celestial-body "Pluto" 1024 0.025))

(define (make-rhea)
  (make-celestial-body "Rhea" 1024 0.025))

(define (make-saturn)
  (let* ((body::Geometry (make-celestial-body "Saturn" 3072 0.05))
         (ring-top::Geometry (make-planetary-ring "Saturn's Ring Top"
                                                  "saturn_ring_alpha.png" 12288 0.05))
         (ring-bottom::Geometry (make-planetary-ring "Saturn's Ring Bottom"
                                                     "saturn_ring_alpha.png" 12288 0.05 bottom: #t))
         (pivot::Node (Node "Saturn")))
    (*:attachChild pivot body)
    (*:attachChild pivot ring-top)
    (*:attachChild pivot ring-bottom)
    pivot))

(define (make-sedna)
  (make-celestial-body "Sedna" 1024 0.025))

(define (make-tethys)
  (make-celestial-body "Tethys" 1024 0.025))

(define (make-the-moon)
  (make-celestial-body "The Moon" 1024 0.025))

(define (make-the-sun)
  (make-celestial-body "The Sun" 8192 0.025))

(define (make-titan)
  (make-celestial-body "Titan" 1024 0.025))

(define (make-uranus)
  (let* ((body::Geometry (make-celestial-body "Uranus" 3072 0.08))
         (ring-top::Geometry (make-planetary-ring "Uranus' Ring Top"
                                                  "uranus_ring_alpha.png" 12288 0.05))
         (ring-bottom::Geometry (make-planetary-ring "Uranus' Ring Bottom"
                                                     "uranus_ring_alpha.png" 12288 0.05 bottom: #t))
         (pivot::Node (Node "Uranus")))
    (*:attachChild pivot body)
    (*:attachChild pivot ring-top)
    (*:attachChild pivot ring-bottom)
    pivot))

(define (make-venus)
  (make-celestial-body "Venus" 2048 0.025))

(define (make-location name::String)
  (cond
   ((string=? name "Callisto")(make-callisto))
   ((string=? name "Dione")(make-dione))
   ((string=? name "Earth")(make-earth))
   ((string=? name "Enceladus")(make-enceladus))
   ((string=? name "Europa")(make-europa))
   ((string=? name "Ganymede")(make-ganymede))
   ((string=? name "Iapetus")(make-iapetus))
   ((string=? name "Io")(make-io))
   ((string=? name "Jupiter")(make-jupiter))
   ((string=? name "Mars")(make-mars))
   ((string=? name "Mercury")(make-mercury))
   ((string=? name "Neptune")(make-neptune))
   ((string=? name "Pluto")(make-pluto))
   ((string=? name "Rhea")(make-rhea))
   ((string=? name "Saturn")(make-saturn))
   ((string=? name "Sedna")(make-sedna))
   ((string=? name "Tethys")(make-tethys))
   ((string=? name "The Moon")(make-the-moon))
   ((string=? name "The Sun")(make-the-sun))
   ((string=? name "Titan")(make-titan))
   ((string=? name "Uranus")(make-uranus))
   ((string=? name "Venus")(make-venus))
   (else (error "Unknown location" name))))
