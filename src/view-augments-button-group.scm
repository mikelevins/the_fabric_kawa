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

(import-as Button tonegod.gui.controls.buttons.Button)
(import-as RadioButtonGroup tonegod.gui.controls.buttons.RadioButtonGroup)
(import-as Screen tonegod.gui.core.Screen)

(define-simple-class AugmentsButtonGroup (RadioButtonGroup)
  (app-state init-form: #!null)
  ((*init* screen::Screen uid::String)
   (invoke-special RadioButtonGroup (this) '*init* screen uid))
  ((onSelect index::int value::Button)
   (let ((button-id (*:getUID value))
         (state::CreateCharacterState app-state))
     (cond
      ((equal? "ForceButton" button-id)(set! state:augment 'force))
      ((equal? "OpticsButton" button-id)(set! state:augment 'optics))
      ((equal? "PortalsButton" button-id)(set! state:augment 'portals))
      ((equal? "TurretsButton" button-id)(set! state:augment 'turrets))
      (else (format #t "~%Unknown augment selected"))))))
