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

(require "util-java.scm")
(require "gamestates.scm")
(require "syntax-classes.scm")

(import-as Button tonegod.gui.controls.buttons.Button)
(import-as RadioButtonGroup tonegod.gui.controls.buttons.RadioButtonGroup)
(import-as Screen tonegod.gui.core.Screen)

(defclass AugmentsButtonGroup (RadioButtonGroup)
  (slots:
   (app-state init-form: #!null getter: getAppState setter: setAppState))
  (methods:
   ((*init* screen::Screen uid::String)
    (invoke-special RadioButtonGroup (this) '*init* screen uid))
   ((onSelect index::int value::Button)
    (let ((button-id (*:getUID value))
          (state::CreateCharacterGameState (*:getAppState (this))))
      (cond
       ((equal? "ForceButton" button-id)(*:setAugment state 'force))
       ((equal? "OpticsButton" button-id)(*:setAugment state 'optics))
       ((equal? "PortalsButton" button-id)(*:setAugment state 'portals))
       ((equal? "TurretsButton" button-id)(*:setAugment state 'turrets))
       (else (format #t "~%Unknown augment selected")))))))
