;;;; ***********************************************************************
;;;;
;;;; Name:          view-character-nameplate.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the character nameplate for the character creator
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 make-character-nameplate)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as BitmapFont com.jme3.font.BitmapFont)
(import-as ColorRGBA com.jme3.math.ColorRGBA)
(import-as Label tonegod.gui.controls.text.Label)
(import-as Screen tonegod.gui.core.Screen)
(import-as Vector2f com.jme3.math.Vector2f)

;;; ---------------------------------------------------------------------
;;; character nameplate
;;; ---------------------------------------------------------------------

(define (make-character-nameplate screen::Screen)
  (let ((character-nameplate (Label screen "CharacterNameplate" (Vector2f 32 960)(Vector2f 1200 40)))
        (Align BitmapFont:Align))
    (*:setText character-nameplate "Character: ")
    (*:setTextAlign character-nameplate Align:Left)
    (*:setFont character-nameplate "Interface/Fonts/Laconic30.fnt")
    (*:setFontSize character-nameplate 24)
    (*:setFontColor character-nameplate ColorRGBA:Green)
    character-nameplate))
