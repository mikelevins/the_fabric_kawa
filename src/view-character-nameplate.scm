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

(define (make-character-nameplate screen::Screen position::Vector2f)
  (let* ((Align BitmapFont:Align)
         (nameplate-left (*:getX position))
         (nameplate-top (*:getY position))
         (character-nameplate (Label screen "CharacterNameplate"
                                     (Vector2f nameplate-left nameplate-top)
                                     (Vector2f 1200 40)
                                     text: ""
                                     textAlign: Align:Left
                                     fontSize: 30
                                     fontColor: ColorRGBA:Green
                                     font:  "Interface/Fonts/Laconic30.fnt")))
    character-nameplate))

