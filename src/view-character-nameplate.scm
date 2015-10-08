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

(require state-create-character)
(require model-rect)
(require view-name-generator)
(require view-armor-picker)
(require view-action-bar)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class com.jme3.font BitmapFont BitmapText))
(import (class com.jme3.math ColorRGBA))
(import (class tonegod.gui.controls.text Label))
(import (class tonegod.gui.core Screen))
(import (class com.jme3.math Vector2f))

;;; ---------------------------------------------------------------------
;;; character nameplate
;;; ---------------------------------------------------------------------

(define (make-character-nameplate screen::Screen)
  (let* ((rect (compute-action-bar-rect screen))
         (Align BitmapFont:Align)
         (nameplate-left (get-left rect))
         (nameplate-top (+ (get-height rect) 16))
         (character-nameplate (Label screen "CharacterNameplate"
                                     (Vector2f nameplate-left nameplate-top)
                                     (Vector2f 1200 40))))
    (*:setText character-nameplate "")
    (*:setTextAlign character-nameplate Align:Left)
    (*:setFont character-nameplate "Interface/Fonts/Laconic30.fnt")
    (*:setFontSize character-nameplate 30)
    (*:setFontColor character-nameplate ColorRGBA:Green)
    character-nameplate))

