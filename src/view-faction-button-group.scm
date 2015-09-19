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

(define bright-caretakers-color (make-parameter (ColorRGBA 0.4 0.8 0.4 0.3)))
(define dim-caretakers-color (make-parameter (ColorRGBA 0.2 0.6 0.2 0.3)))
(define bright-rogues-color (make-parameter (ColorRGBA 0.2 0.6 0.8 0.3)))
(define dim-rogues-color (make-parameter (ColorRGBA 0.0 0.3 0.6 0.3)))
(define bright-abjurers-color (make-parameter (ColorRGBA 0.8 0.0 0.0 0.2)))
(define dim-abjurers-color (make-parameter (ColorRGBA 0.6 0.0 0.0 0.2)))

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
       (begin (set! state:faction 'caretakers)
              (*:setText nameplate "Faction: Caretakers")
              (set! lit-color (bright-caretakers-color))
              (set! dim-color (bright-caretakers-color))))
      ((equal? "RoguesButton" button-id)
       (begin (set! state:faction 'rogues)
              (*:setText nameplate "Faction: Rogues")
              (set! lit-color (bright-rogues-color))
              (set! dim-color (bright-rogues-color))))
      ((equal? "AbjurersButton" button-id)
       (begin (set! state:faction 'abjurers)
              (*:setText nameplate "Faction: Abjurers")
              (set! lit-color (bright-abjurers-color))
              (set! dim-color (bright-abjurers-color))))
      (else (format #t "~%Unknown faction selected")))
     (recolor-character-model! character lit-color dim-color))))
