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
(require "view-player-character.scm")
(require "client-main.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as AbstractAppState com.jme3.app.state.AbstractAppState)
(import-as AppStateManager com.jme3.app.state.AppStateManager)
(import-as AssetManager com.jme3.asset.AssetManager)
(import-as BitmapFont com.jme3.font.BitmapFont)
(import-as Button tonegod.gui.controls.buttons.Button)
(import-as ButtonAdapter tonegod.gui.controls.buttons.ButtonAdapter)
(import-as ColorRGBA com.jme3.math.ColorRGBA)
(import-as Label tonegod.gui.controls.text.Label)
(import-as MouseButtonEvent com.jme3.input.event.MouseButtonEvent)
(import-as RadioButton tonegod.gui.controls.buttons.RadioButton)
(import-as RadioButtonGroup tonegod.gui.controls.buttons.RadioButtonGroup)
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

(defclass FactionButtonGroup (RadioButtonGroup)
  (methods:
   ((*init* screen::Screen uid::String)
    (invoke-special RadioButtonGroup (this) '*init* screen uid))
   ((onSelect index::int value::Button)
    (format #t "button selected: ~A" value))))

(defclass CharacterCreatorAppState (AbstractAppState)
  (slots:
   (app::SimpleApplication init-form: #!null getter: getApp setter: setApp)
   (state-manager::AppStateManager init-form: #!null getter: getStateManager setter: setStateManager))
  (methods:
   ((initialize mgr::AppStateManager client::SimpleApplication)
    (*:setApp (this) client)
    (let* ((screen (Screen client))
           (gui-node (*:getGuiNode client))
           (root-node (*:getRootNode (as SimpleApplication app)))
           (align BitmapFont:Align)
           (valign BitmapFont:VAlign)
           (sky (make-sky app))
           (faction-palette (Window screen "FactionPalette" (Vector2f 10 10)(Vector2f 400 232)))
           (faction-group (FactionButtonGroup screen "FactionGroup"))
           (highlight-color (ColorRGBA 1.0 1.0 0.0 1.0))
           (caretaker-button (RadioButton screen "CaretakerButton"(Vector2f 8 32)(Vector2f 128 128)))
           (abjurer-button (RadioButton screen "AbjurerButton"(Vector2f 136 32)(Vector2f 128 128)))
           (rogue-button (RadioButton screen "RogueButton"(Vector2f 264 32)(Vector2f 128 128)))
           (character (make-player-character))
           (char-cube (get-property character 'cube: default: #f)))
      (*:attachChild root-node sky)
      (if char-cube
          (*:attachChild root-node char-cube))
      (*:setWindowTitle faction-palette "Choose a Faction:")
      (*:setButtonIcon caretaker-button 128 128 "Interface/caretaker_icon.png")
      (*:setText caretaker-button "Caretakers")
      (*:setTextAlign caretaker-button align:Center)
      (*:setTextVAlign caretaker-button valign:Bottom)
      (*:setFontSize caretaker-button 18)
      (*:addButton faction-group caretaker-button)
      (*:setButtonIcon abjurer-button 128 128 "Interface/abjurer_icon.png")
      (*:setText abjurer-button "Abjurers")
      (*:setTextAlign abjurer-button align:Center)
      (*:setTextVAlign abjurer-button valign:Bottom)
      (*:setFontSize abjurer-button 18)
      (*:addButton faction-group abjurer-button)
      (*:setButtonIcon rogue-button 128 128 "Interface/rogue_icon.png")
      (*:setText rogue-button "Rogues")
      (*:setTextAlign rogue-button align:Center)
      (*:setTextVAlign rogue-button valign:Bottom)
      (*:setFontSize rogue-button 18)
      (*:addButton faction-group rogue-button)
      (*:addChild faction-palette caretaker-button)
      (*:addChild faction-palette abjurer-button)
      (*:addChild faction-palette rogue-button)
      (*:addElement screen faction-palette)
      (*:addControl gui-node screen)))))
