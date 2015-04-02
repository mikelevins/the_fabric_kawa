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
(require "client-states.scm")
(require "syntax-classes.scm")

(import-as Button tonegod.gui.controls.buttons.Button)
(import-as RadioButtonGroup tonegod.gui.controls.buttons.RadioButtonGroup)
(import-as Screen tonegod.gui.core.Screen)

(defclass WeaponsButtonGroup (RadioButtonGroup)
  (slots:
   (app-state init-form: #!null getter: getAppState setter: setAppState))
  (methods:
   ((*init* screen::Screen uid::String)
    (invoke-special RadioButtonGroup (this) '*init* screen uid))
   ((onSelect index::int value::Button)
    (let ((button-id (*:getUID value))
          (state::CreateCharacterClientState (*:getAppState (this))))
      (cond
       ((equal? "CannonButton" button-id)(*:setWeapon state 'cannon))
       ((equal? "ImpulseButton" button-id)(*:setWeapon state 'impulse))
       ((equal? "MalwareButton" button-id)(*:setWeapon state 'malware))
       ((equal? "BotsButton" button-id)(*:setWeapon state 'bots))
       (else (format #t "~%Unknown weapon selected")))))))
