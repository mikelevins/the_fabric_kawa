;;;; ***********************************************************************
;;;;
;;;; Name:          view-augments-button-group.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       radio-button group for the augment picker
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 AugmentButtonGroup)

(require client)
(require state-create-character)
(require model-character)

(import (class tonegod.gui.controls.buttons Button RadioButtonGroup))
(import (class tonegod.gui.core Screen))

(define-simple-class AugmentButtonGroup (RadioButtonGroup)
  (state init-form: #!null)
  ((*init* screen::Screen uid::String)
   (invoke-special RadioButtonGroup (this) '*init* screen uid))
  ((onSelect index::int value::Button)
   (let* ((button-id (*:getUID value))
          (state::CreateCharacterState state)
          (character::FabricCharacter state:character))
     (cond
      ((equal? "ForceButton" button-id)(set-character-augment! state 'force))
      ((equal? "OpticsButton" button-id)(set-character-augment! state 'optics))
      ((equal? "PortalsButton" button-id)(set-character-augment! state 'portals))
      ((equal? "TurretsButton" button-id)(set-character-augment! state 'turrets))
      (else (format #t "~%Unknown augment selected"))))))

