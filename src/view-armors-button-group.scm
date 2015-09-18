;;;; ***********************************************************************
;;;;
;;;; Name:          view-armors-button-group.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       radio-button group for the armor picker
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 ArmorsButtonGroup)

(require util-java)
(require client-state)
(require state-create-character)

(import-as Button tonegod.gui.controls.buttons.Button)
(import-as RadioButtonGroup tonegod.gui.controls.buttons.RadioButtonGroup)
(import-as Screen tonegod.gui.core.Screen)

(define-simple-class  ArmorsButtonGroup (RadioButtonGroup)
  (app-state init-form: #!null)
  ((*init* screen::Screen uid::String)
   (invoke-special RadioButtonGroup (this) '*init* screen uid))
  ((onSelect index::int value::Button)
   (let ((button-id (*:getUID value))
         (state::CreateCharacterState app-state))
     (cond
      ((equal? "AbsorbButton" button-id)(set!  state:armor 'absorb))
      ((equal? "RegenerateButton" button-id)(set! state:armor 'regenerate))
      ((equal? "PowerButton" button-id)(set! state:armor 'power))
      ((equal? "EnergyButton" button-id)(set! state:armor 'energy))
      (else (format #t "~%Unknown armor selected"))))))
