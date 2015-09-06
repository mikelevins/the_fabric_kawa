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

(require "util-java.scm")
(require "client-state.scm")
(require "state-create-character.scm")

(import-as Button tonegod.gui.controls.buttons.Button)
(import-as RadioButtonGroup tonegod.gui.controls.buttons.RadioButtonGroup)
(import-as Screen tonegod.gui.core.Screen)

(define-simple-class WeaponsButtonGroup (RadioButtonGroup)
  (app-state init-form: #!null)
  ((*init* screen::Screen uid::String)
   (invoke-special RadioButtonGroup (this) '*init* screen uid))
  ((onSelect index::int value::Button)
   (let ((button-id (*:getUID value))
         (state::CreateCharacterState app-state))
     (cond
      ((equal? "CannonButton" button-id)(set! state:weapon 'cannon))
      ((equal? "ImpulseButton" button-id)(set! state:weapon 'impulse))
      ((equal? "MalwareButton" button-id)(set! state:weapon 'malware))
      ((equal? "BotsButton" button-id)(set! state:weapon 'bots))
      (else (format #t "~%Unknown weapon selected"))))))

