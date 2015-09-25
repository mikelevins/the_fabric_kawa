;;;; ***********************************************************************
;;;;
;;;; Name:          view-armor-button-group.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       radio-button group for the armor picker
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 ArmorButtonGroup)

(require state)
(require state-create-character)
(require model-character)

(import (class tonegod.gui.controls.buttons Button RadioButtonGroup))
(import (class tonegod.gui.core Screen))

(define-simple-class  ArmorButtonGroup (RadioButtonGroup)
  (state::CreateCharacterState init-form: #!null)
  ((*init* screen::Screen uid::String)
   (invoke-special RadioButtonGroup (this) '*init* screen uid))
  ((onSelect index::int value::Button)
   (let* ((button-id (*:getUID value)))
     (cond
      ((equal? "AbsorbButton" button-id)(set-character-armor! state 'absorb))
      ((equal? "RegenerateButton" button-id)(set-character-armor! state 'regenerate))
      ((equal? "PowerButton" button-id)(set-character-armor! state 'power))
      ((equal? "EnergyButton" button-id)(set-character-armor! state 'energy))
      (else (format #t "~%Unknown armor selected"))))))

