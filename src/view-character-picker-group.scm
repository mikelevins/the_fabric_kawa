;;;; ***********************************************************************
;;;;
;;;; Name:          view-character-picker-group.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       radio button group for picking characters
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 CharacterPickerGroup)

(import (class java.lang String))
(import (class tonegod.gui.controls.buttons Button RadioButtonGroup))
(import (class tonegod.gui.core Screen))

(define-simple-class CharacterPickerGroup (RadioButtonGroup)
  ;; slots
  (state init-form: #!null)
  ;; methods
  ((*init* screen::Screen uid::String)
   (invoke-special RadioButtonGroup (this) '*init* screen uid))
  ((onSelect index::int value::Button)
   (format #t "~%CharacterPickerGroup onSelect executed")))
