;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          view-node.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       game nodes
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export make-sky make-center-body)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; a node is a specific places on the game's network map of the
;;; solar system. Each node has an ID number, a location on the
;;; network graph, a set of routes to other nodes, a skybox, and a
;;; set of contents, including artifacts, spawns, hubs, and NPCs.
;;;
;;; for now, each node has a central celestial object, usually a
;;; planet or moon. in the long run we may want to change this
;;; to achieve a few goals:
;;; - being able to have multiple nodes per celestial object
;;; - being able to make the celestial objects really big, and
;;;   prevent players from intersecting with them (we can, for example,
;;;   place within the frustum, but outside the area of travel)

(require "assets-general.scm")
(require "util-java.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(define-private-alias AssetManager com.jme3.asset.AssetManager)
(define-private-alias SimpleApplication com.jme3.app.SimpleApplication)
(define-private-alias SkyFactory com.jme3.util.SkyFactory)
(define-private-alias Sphere com.jme3.scene.shape.Sphere)


;;; (make-sky app ::SimpleApplication)
;;; ---------------------------------------------------------------------

(define (make-sky app ::SimpleApplication)
  (let ((asset-manager::AssetManager (get-asset-manager)))
    (SkyFactory:createSky asset-manager 
                          (@ 'loadTexture asset-manager "Textures/tycholeft.png")
                          (@ 'loadTexture asset-manager "Textures/tychoright.png")
                          (@ 'loadTexture asset-manager "Textures/tychofront.png")
                          (@ 'loadTexture asset-manager "Textures/tychoback.png")
                          (@ 'loadTexture asset-manager "Textures/tychotop.png")
                          (@ 'loadTexture asset-manager "Textures/tychobottom.png"))))


;;; (make-center-body app ::SimpleApplication)
;;; ---------------------------------------------------------------------

(define (make-center-body app ::SimpleApplication body-name::java.lang.String)
  (let* ((asset-manager::AssetManager (get-asset-manager))
         (center::Sphere (Sphere 128 128 2048.0))
         (center-rotator (<rotate-control> 0.0 0.0 0.05))
         (center-geom (com.jme3.scene.Geometry "Center" center))
         (center-mat (com.jme3.material.Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
         (center-tex (@ 'loadTexture asset-manager (string-append "Textures/" body-name ".png"))))
    ;; set up the look of the celestial object at the center of the skybox
    (invoke center 'setTextureMode Sphere:TextureMode:Projected)
    (@ 'setTexture center-mat "ColorMap" center-tex)
    (@ 'setMaterial center-geom center-mat)
    (@ 'addControl center-geom center-rotator)
    center-geom))


