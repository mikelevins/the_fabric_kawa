;;;; ***********************************************************************
;;;; Name:          setting-celestial-objects.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       constructing celestial objects
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export make-celestial-object)

(require "utilities-math.scm")
(require "assets-general.scm")

(define-private-alias AssetManager com.jme3.asset.AssetManager)
(define-private-alias Geometry com.jme3.scene.Geometry)
(define-private-alias Material com.jme3.material.Material)
(define-private-alias Sphere com.jme3.scene.shape.Sphere)

(define (make-celestial-object object-name)
  (let* ((asset-manager::AssetManager (get-asset-manager))
         (object::Sphere (Sphere 128 128 2048.0))
         (object-geom (Geometry object-name object))
         (object-mat (Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
         (object-tex (*:loadTexture asset-manager (string-append "Textures/" object-name ".jpg"))))
    (*:setTextureMode object Sphere:TextureMode:Projected)
    (*:setTexture object-mat "ColorMap" object-tex)
    (*:setMaterial object-geom object-mat)
    (*:center object-geom)
    ;;(*:rotate object-geom (degrees->radians 0) 0 0)
    object-geom))
