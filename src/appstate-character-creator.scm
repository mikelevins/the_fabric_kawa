;;;; ***********************************************************************
;;;;
;;;; Name:          appstate-character-creator.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       create a new character
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export CharacterCreatorAppState)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "syntax-classes.scm")
(require "client-main.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as AbstractAppState com.jme3.app.state.AbstractAppState)
(import-as AppStateManager com.jme3.app.state.AppStateManager)
(import-as AssetManager com.jme3.asset.AssetManager)
(import-as ButtonAdapter tonegod.gui.controls.buttons.ButtonAdapter)
(import-as Label tonegod.gui.controls.text.Label)
(import-as MouseButtonEvent com.jme3.input.event.MouseButtonEvent)
(import-as Screen tonegod.gui.core.Screen)
(import-as Serializer com.jme3.network.serializing.Serializer)
(import-as SimpleApplication com.jme3.app.SimpleApplication)
(import-as SkyFactory com.jme3.util.SkyFactory)
(import-as Vector2f com.jme3.math.Vector2f)
(import-as Vector4f com.jme3.math.Vector4f)
(import-as Window tonegod.gui.controls.windows.Window)

;;; ---------------------------------------------------------------------
;;; the CharacterCreatorAppState class
;;; ---------------------------------------------------------------------

;;; (make-sky app ::SimpleApplication)
;;; ---------------------------------------------------------------------

(define (make-sky app::SimpleApplication)
  (let ((asset-manager::AssetManager (get-asset-manager)))
    (SkyFactory:createSky asset-manager 
                          (*:loadTexture asset-manager "Textures/tycholeft.png")
                          (*:loadTexture asset-manager "Textures/tychoright.png")
                          (*:loadTexture asset-manager "Textures/tychofront.png")
                          (*:loadTexture asset-manager "Textures/tychoback.png")
                          (*:loadTexture asset-manager "Textures/tychotop.png")
                          (*:loadTexture asset-manager "Textures/tychobottom.png"))))


(defclass CharacterCreatorAppState (AbstractAppState)
  (slots:
   (app::SimpleApplication init-form: #!null getter: getApp setter: setApp)
   (state-manager::AppStateManager init-form: #!null getter: getStateManager setter: setStateManager))
  (methods:
   ((initialize mgr::AppStateManager client::SimpleApplication)
    (*:setApp (this) client)
    (let* ((screen (Screen client))
           (gui-node (*:getGuiNode client))
           (root-node (*:getRootNode app))
           (sky (make-sky app)))
      (*:attachChild root-node sky)
      (*:addControl gui-node screen)))))
