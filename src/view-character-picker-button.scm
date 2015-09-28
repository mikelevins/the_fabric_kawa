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
 CharacterPickerButton)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require state-pick-character)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class com.jme3.math Vector2f))
(import (class tonegod.gui.controls.buttons Button RadioButton))
(import (class tonegod.gui.core Screen))

;;; CLASS 
;;; ---------------------------------------------------------------------
;;; a RadioButton subclass used to pick a character to play

(define-simple-class CharacterPickerButton (RadioButton)
  (state::PickCharacterState init-form: #!null)
  (character init-form: #!null)
  ((*init* a-state::PickCharacterState a-character::FabricCharacter
           screen::Screen uid::String position::Vector2f size::Vector2f)
   (invoke-special RadioButton (this) '*init* screen uid position size)
   (set! state a-state)
   (set! character a-character)))
