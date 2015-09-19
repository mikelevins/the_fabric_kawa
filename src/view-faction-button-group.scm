;;;; ***********************************************************************
;;;;
;;;; Name:          view-faction-picker.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       a UI element for picking the faction of a newly-created character
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 FactionButtonGroup)

(require util-java)
(require client-state)
(require state-create-character)

(import-as Button tonegod.gui.controls.buttons.Button)
(import-as Label tonegod.gui.controls.text.Label)
(import-as RadioButtonGroup tonegod.gui.controls.buttons.RadioButtonGroup)
(import-as Screen tonegod.gui.core.Screen)

(define-simple-class FactionButtonGroup (RadioButtonGroup)
  (app-state init-form: #!null)
  ((*init* screen::Screen uid::String)
   (invoke-special RadioButtonGroup (this) '*init* screen uid))
  ((onSelect index::int value::Button)
   (let* ((button-id (*:getUID value))
          (state::CreateCharacterState app-state)
          (caretakers-button::Button state:caretakers-button)
          (rogues-button::Button state:rogues-button)
          (abjurers-button::Button state:abjurers-button)
          (nameplate::Label state:faction-nameplate))
     (cond
      ((equal? "CaretakerButton" button-id)
       (begin (set! state:faction 'caretakers)
              (*:setText nameplate "Faction: Caretakers")))
      ((equal? "RoguesButton" button-id)
       (begin (set! state:faction 'rogues)
              (*:setText nameplate "Faction: Rogues")))
      ((equal? "AbjurersButton" button-id)
       (begin (set! state:faction 'abjurers)
              (*:setText nameplate "Faction: Abjurers")))
      (else (format #t "~%Unknown faction selected"))))))
