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

(require util-color)
(require client-state)
(require state-create-character)
(require model-character)

(import (class com.jme3.math ColorRGBA))
(import (class com.jme3.scene Node))
(import (class tonegod.gui.controls.buttons Button RadioButtonGroup))
(import (class tonegod.gui.controls.text Label))
(import (class tonegod.gui.core Screen))

(define-simple-class FactionButtonGroup (RadioButtonGroup)
  (app-state init-form: #!null)
  ((*init* screen::Screen uid::String)
   (invoke-special RadioButtonGroup (this) '*init* screen uid))
  ((onSelect index::int value::Button)
   (let* ((button-id (*:getUID value))
          (state::CreateCharacterState app-state)
          (character::FabricCharacter state:character)
          (model::Node character:model)
          (caretakers-button::Button state:caretakers-button)
          (rogues-button::Button state:rogues-button)
          (abjurers-button::Button state:abjurers-button)
          (nameplate::Label state:faction-nameplate))
     (cond
      ((equal? "CaretakerButton" button-id)
       (begin (set! character:faction 'caretakers)
              (*:setText nameplate "Faction: Caretakers")))
      ((equal? "RoguesButton" button-id)
       (begin (set! character:faction 'rogues)
              (*:setText nameplate "Faction: Rogues")))
      ((equal? "AbjurersButton" button-id)
       (begin (set! character:faction 'abjurers)
              (*:setText nameplate "Faction: Abjurers")))
      (else (format #t "~%Unknown faction selected")))
     (let ((lit-color::ColorRGBA (faction-lit-color character:faction))
           (dim-color::ColorRGBA (faction-dim-color character:faction)))
       (recolor-character-model! character lit-color dim-color)))))
