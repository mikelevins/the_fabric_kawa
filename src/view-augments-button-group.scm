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
 AugmentsButtonGroup)

(require util-java)
(require client-state)
(require state-create-character)
(require model-character)

(import-as Button tonegod.gui.controls.buttons.Button)
(import-as RadioButtonGroup tonegod.gui.controls.buttons.RadioButtonGroup)
(import-as Screen tonegod.gui.core.Screen)

(define-simple-class AugmentsButtonGroup (RadioButtonGroup)
  (app-state init-form: #!null)
  ((*init* screen::Screen uid::String)
   (invoke-special RadioButtonGroup (this) '*init* screen uid))
  ((onSelect index::int value::Button)
   (let* ((button-id (*:getUID value))
          (state::CreateCharacterState app-state)
          (character::FabricCharacter state:character))
     (cond
      ((equal? "ForceButton" button-id)(set-character-augment! character 'force))
      ((equal? "OpticsButton" button-id)(set-character-augment! character 'optics))
      ((equal? "PortalsButton" button-id)(set-character-augment! character 'portals))
      ((equal? "TurretsButton" button-id)(set-character-augment! character 'turrets))
      (else (format #t "~%Unknown augment selected"))))))

