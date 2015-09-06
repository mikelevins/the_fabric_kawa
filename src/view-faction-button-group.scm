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

(require "util-java.scm")
(require "client-state.scm")
(require "state-create-character.scm")

(import-as Button tonegod.gui.controls.buttons.Button)
(import-as RadioButtonGroup tonegod.gui.controls.buttons.RadioButtonGroup)
(import-as Screen tonegod.gui.core.Screen)

(define-simple-class FactionButtonGroup (RadioButtonGroup)
  (app-state init-form: #!null)
  ((*init* screen::Screen uid::String)
   (invoke-special RadioButtonGroup (this) '*init* screen uid))
  ((onSelect index::int value::Button)
   (let ((button-id (*:getUID value))
         (state::CreateCharacterState app-state))
     (cond
      ((equal? "CaretakerButton" button-id)(set! state:faction 'caretakers))
      ((equal? "RoguesButton" button-id)(set! state:faction 'rogues))
      ((equal? "AbjurersButton" button-id)(set! state:faction 'abjurers))
      (else (format #t "~%Unknown faction selected"))))))
