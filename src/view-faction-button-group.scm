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
(require util-color)
(require client-state)
(require state-create-character)
(require model-character)

(import-as Button tonegod.gui.controls.buttons.Button)
(import-as ColorRGBA com.jme3.math.ColorRGBA)
(import-as Label tonegod.gui.controls.text.Label)
(import-as Node com.jme3.scene.Node)
(import-as RadioButtonGroup tonegod.gui.controls.buttons.RadioButtonGroup)
(import-as Screen tonegod.gui.core.Screen)

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
          (nameplate::Label state:faction-nameplate)
          (lit-color::ColorRGBA #!null)
          (dim-color::ColorRGBA #!null))
     (cond
      ((equal? "CaretakerButton" button-id)
       (begin (set! character:faction 'caretakers)
              (*:setText nameplate "Faction: Caretakers")
              (set! lit-color (bright-caretakers-color))
              (set! dim-color (dim-caretakers-color))))
      ((equal? "RoguesButton" button-id)
       (begin (set! character:faction 'rogues)
              (*:setText nameplate "Faction: Rogues")
              (set! lit-color (bright-rogues-color))
              (set! dim-color (dim-rogues-color))))
      ((equal? "AbjurersButton" button-id)
       (begin (set! character:faction 'abjurers)
              (*:setText nameplate "Faction: Abjurers")
              (set! lit-color (bright-abjurers-color))
              (set! dim-color (dim-abjurers-color))))
      (else (format #t "~%Unknown faction selected")))
     (recolor-character-model! character lit-color dim-color))))
