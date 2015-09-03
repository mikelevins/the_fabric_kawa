;;;; ***********************************************************************
;;;;
;;;; Name:          view-faction-nameplate.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the faction nameplate for the character creator
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 make-faction-nameplate)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "state-create-character.scm")
(require "model-rect.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as BitmapFont com.jme3.font.BitmapFont)
(import-as BitmapText com.jme3.font.BitmapText)
(import-as ColorRGBA com.jme3.math.ColorRGBA)
(import-as Label tonegod.gui.controls.text.Label)
(import-as Screen tonegod.gui.core.Screen)
(import-as Vector2f com.jme3.math.Vector2f)

;;; ---------------------------------------------------------------------
;;; faction nameplate
;;; ---------------------------------------------------------------------

(define (make-faction-nameplate screen::Screen)
  (let* ((faction-picker-rect (compute-faction-picker-rect screen))
         (screen-width (*:getWidth screen))
         (Align BitmapFont:Align)
         (nameplate-left (+ (get-left faction-picker-rect)
                            (get-width faction-picker-rect)
                            16))
         (nameplate-top (get-top faction-picker-rect))
         (nameplate-width (- screen-width (* 2 nameplate-left)))
         (faction-nameplate (Label screen "FactionNameplate"
                                   (Vector2f nameplate-left nameplate-top)
                                   (Vector2f nameplate-width 40))))
    (*:setText faction-nameplate "Faction: ")
    (*:setTextAlign faction-nameplate Align:Left)
    (*:setFont faction-nameplate "Interface/Fonts/Laconic30.fnt")
    (*:setFontSize faction-nameplate 30)
    (*:setFontColor faction-nameplate ColorRGBA:Green)
    faction-nameplate))
