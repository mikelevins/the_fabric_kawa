;;;; ***********************************************************************
;;;;
;;;; Name:          view-weapon-button-group.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       radio-button group for the weapon picker
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 WeaponButtonGroup)

(require state)
(require model-character)
(require state-create-character)
(require client)

(import (class tonegod.gui.controls.buttons Button RadioButtonGroup))
(import (class tonegod.gui.core Screen))

(define-simple-class WeaponButtonGroup (RadioButtonGroup)
  (state::CreateCharacterState init-form: #!null)
  ((*init* screen::Screen uid::String)
   (invoke-special RadioButtonGroup (this) '*init* screen uid))
  ((onSelect index::int value::Button)
   (let* ((button-id (*:getUID value))
          (character::FabricCharacter (current-character)))
     (cond
      ((equal? "CannonButton" button-id)(set-character-weapon! state 'cannon))
      ((equal? "ImpulseButton" button-id)(set-character-weapon! state 'impulse))
      ((equal? "MalwareButton" button-id)(set-character-weapon! state 'malware))
      ((equal? "BotsButton" button-id)(set-character-weapon! state 'bots))
      (else (format #t "~%Unknown weapon selected"))))))

