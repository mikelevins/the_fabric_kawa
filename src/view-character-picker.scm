;;;; ***********************************************************************
;;;;
;;;; Name:          view-character-picker.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the character picker 
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 compute-character-picker-rect
 make-character-picker)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require state)
(require state-pick-character)
(require model-rect)
(require view-character-picker-group)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class com.jme3.font BitmapFont))
(import (class com.jme3.math ColorRGBA Vector2f))
(import (class tonegod.gui.controls.buttons Button RadioButton))
(import (class tonegod.gui.controls.windows Panel))
(import (class tonegod.gui.core Screen))

;;; ---------------------------------------------------------------------
;;; 
;;; ---------------------------------------------------------------------

(define (compute-character-picker-rect screen::Screen)
  (let* ((screen-height (*:getHeight screen)))
    (make-rectangle 16 16 512 (- screen-height 32))))

(define (make-character-picker-buttons state::PickCharacterState screen::Screen)
  '())

(define (make-character-picker state::PickCharacterState screen::Screen)
  (let* ((rect (compute-character-picker-rect screen))
         (align BitmapFont:Align)
         (valign BitmapFont:VAlign)
         (win::Panel (Panel screen "CharacterPicker"
                            (Vector2f (get-left rect)(get-top rect))
                            (Vector2f (get-width rect)(get-height rect))))
         (character-group (CharacterPickerGroup screen "CharacterGroup"))
         (picker-buttons (make-character-picker-buttons state screen)))
    
    (set! character-group:state state)
    win))
