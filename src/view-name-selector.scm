;;;; ***********************************************************************
;;;;
;;;; Name:          view-name-selector.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       a UI element for picking a name part
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 NameSelector)

(require "util-java.scm")
(require "data-assets.scm")
(require "client-state.scm")

(import-as AssetManager com.jme3.asset.AssetManager)
(import-as Button tonegod.gui.controls.buttons.Button)
(import-as ColorRGBA com.jme3.math.ColorRGBA)
(import-as Material com.jme3.material.Material)
(import-as RadioButtonGroup tonegod.gui.controls.buttons.RadioButtonGroup)
(import-as Screen tonegod.gui.core.Screen)
(import-as SelectList tonegod.gui.controls.lists.SelectList)
(import-as Texture com.jme3.texture.Texture)
(import-as Vector2f com.jme3.math.Vector2f)

(define-simple-class NameSelector (SelectList)
  ((*init* screen::Screen uid::String position::Vector2f size::Vector2f)
   (invoke-special SelectList (this) '*init* screen uid position size)
   (let* ((asset-manager::AssetManager (get-asset-manager))
          (img::Texture (*:loadTexture asset-manager "Textures/name_menu_text_field.png"))
          (mat::Material (*:getElementMaterial (this)))
          (scrollarea (*:getScrollableArea (this))))
     (*:setFontColor scrollarea (ColorRGBA 0.0 1.0 0.0 1.0))
     (*:setTexture mat "ColorMap" img)
     (*:setFontSize (this) 24)
     (*:setFontSize scrollarea 24)))
  ((onChange)(format #t "name selection changed"))
  ((getScrollableArea) scrollableArea))
