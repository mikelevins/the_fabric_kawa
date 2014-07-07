;;;; ***********************************************************************
;;;; Name:          ui-client-hud.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       status and heads-up displays
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export init-client-hud)

(define-private-alias BitmapFont com.jme3.font.BitmapFont)
(define-private-alias ColorRGBA com.jme3.math.ColorRGBA)
(define-private-alias Label tonegod.gui.controls.text.Label)
(define-private-alias Screen tonegod.gui.core.Screen)
(define-private-alias Vector2f com.jme3.math.Vector2f)

(define (make-nameplate screen character-name)
  (let ((Align BitmapFont:Align)
        (nameplate (Label screen "Character Name" (Vector2f 8 8)(Vector2f 900 40))))
    (*:setText nameplate character-name)
    (*:setTextAlign nameplate Align:Left)
    (*:setFont nameplate "Interface/Fonts/Laconic30.fnt")
    (*:setFontSize nameplate 30)
    (*:setFontColor nameplate ColorRGBA:Green)
    nameplate))

(define (make-hub-plate screen hub-name)
  (let ((Align BitmapFont:Align)
        (plate (Label screen "Hub Name" (Vector2f 8 48)(Vector2f 900 40))))
    (*:setText plate hub-name)
    (*:setTextAlign plate Align:Left)
    (*:setFont plate "Interface/Fonts/Laconic24.fnt")
    (*:setFontSize plate 24)
    (*:setFontColor plate ColorRGBA:Green)
    plate))

(define (init-client-hud app)
  (let* ((screen ::Screen (get-key app gui-screen:))
         (settings (get-key app settings:))
         (character-name (get-key app character-name:))
         (hub-name (get-key app hub-name:))
         (nameplate (make-nameplate screen character-name))
         (hub-plate (make-hub-plate screen hub-name)))
    (*:addElement screen nameplate)
    (*:addElement screen hub-plate)))
