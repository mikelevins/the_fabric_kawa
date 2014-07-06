;;;; ***********************************************************************
;;;; Name:          setting-sky.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       all known hub names
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export make-client-sky)

(require "assets-general.scm")

(define-private-alias AssetManager com.jme3.asset.AssetManager)
(define-private-alias SkyFactory com.jme3.util.SkyFactory)
(define-private-alias Spatial com.jme3.scene.Spatial)

(define (make-client-sky)
  (let* ((asset-manager::AssetManager (get-asset-manager))
         (sky::Spatial (SkyFactory:createSky
                        asset-manager 
                        (*:loadTexture asset-manager "Textures/tycholeft.png")
                        (*:loadTexture asset-manager "Textures/tychoright.png")
                        (*:loadTexture asset-manager "Textures/tychofront.png")
                        (*:loadTexture asset-manager "Textures/tychoback.png")
                        (*:loadTexture asset-manager "Textures/tychotop.png")
                        (*:loadTexture asset-manager "Textures/tychobottom.png"))))
    (*:setName sky "skybox")
    sky))

