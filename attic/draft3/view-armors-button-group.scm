;;;; ***********************************************************************
;;;;
;;;; Name:          view-armors-button-group.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       radio-button group for the armor picker
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 ArmorsButtonGroup)

(require "util-java.scm")
(require "client-states.scm")
(require "syntax-classes.scm")

(import-as Button tonegod.gui.controls.buttons.Button)
(import-as RadioButtonGroup tonegod.gui.controls.buttons.RadioButtonGroup)
(import-as Screen tonegod.gui.core.Screen)

(defclass ArmorsButtonGroup (RadioButtonGroup)
  (slots:
   (app-state init-form: #!null getter: getAppState setter: setAppState))
  (methods:
   ((*init* screen::Screen uid::String)
    (invoke-special RadioButtonGroup (this) '*init* screen uid))
   ((onSelect index::int value::Button)
    (let ((button-id (*:getUID value))
          (state::CreateCharacterClientState (*:getAppState (this))))
      (cond
       ((equal? "AbsorbButton" button-id)(*:setArmor state 'absorb))
       ((equal? "RegenerateButton" button-id)(*:setArmor state 'regenerate))
       ((equal? "PowerButton" button-id)(*:setArmor state 'power))
       ((equal? "EnergyButton" button-id)(*:setArmor state 'energy))
       (else (format #t "~%Unknown armor selected")))))))
