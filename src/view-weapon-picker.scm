;;;; ***********************************************************************
;;;;
;;;; Name:          view-weapon-picker.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the weapon picker 
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 compute-weapon-picker-rect
 make-weapon-picker)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require state)
(require state-create-character)
(require model-rect)
(require view-faction-picker)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class com.jme3.math ColorRGBA Vector2f))
(import (class tonegod.gui.controls.windows Window))
(import (class tonegod.gui.core Screen))

;;; ---------------------------------------------------------------------
;;; 
;;; ---------------------------------------------------------------------

(define (compute-weapon-picker-rect screen::Screen)
  (let* ((faction-rect (compute-faction-picker-rect screen))
         (screen-width (*:getWidth screen))
         (screen-height (*:getHeight screen))
         (picker-width 256)
         (picker-left (- screen-width (+ picker-width 16)))
         (picker-top (+ 16 (get-top faction-rect)(get-height faction-rect)))
         (picker-height (- screen-height picker-top 16)))
    (make-rectangle picker-left picker-top picker-width picker-height)))

(define (make-weapon-picker state::FabricClientState screen::Screen)
  (let* ((state::CreateCharacterState (as CreateCharacterState state))
         (rect (compute-weapon-picker-rect screen))
         (win (Window screen "WeaponPicker"
                      (Vector2f (get-left rect) (get-top rect))
                      (Vector2f (get-width rect) (get-height rect)))))
    (*:setWindowTitle win "Choose your weapon:")
    win))
