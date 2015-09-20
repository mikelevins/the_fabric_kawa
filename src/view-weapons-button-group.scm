;;;; ***********************************************************************
;;;;
;;;; Name:          view-weapons-button-group.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       radio-button group for the weapon picker
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 WeaponsButtonGroup)

(require util-java)
(require client-state)
(require model-character)
(require state-create-character)

(import (class tonegod.gui.controls.buttons Button RadioButtonGroup))
(import (class tonegod.gui.core Screen))

(define-simple-class WeaponsButtonGroup (RadioButtonGroup)
  (app-state init-form: #!null)
  ((*init* screen::Screen uid::String)
   (invoke-special RadioButtonGroup (this) '*init* screen uid))
  ((onSelect index::int value::Button)
   (let* ((button-id (*:getUID value))
          (state::CreateCharacterState app-state)
          (character::FabricCharacter state:character))
     (cond
      ((equal? "CannonButton" button-id)(set-character-weapon! character 'cannon))
      ((equal? "ImpulseButton" button-id)(set-character-weapon! character 'impulse))
      ((equal? "MalwareButton" button-id)(set-character-weapon! character 'malware))
      ((equal? "BotsButton" button-id)(set-character-weapon! character 'bots))
      (else (format #t "~%Unknown weapon selected"))))))

