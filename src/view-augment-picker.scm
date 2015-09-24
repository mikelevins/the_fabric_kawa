;;;; ***********************************************************************
;;;;
;;;; Name:          view-augment-picker.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the augment picker 
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 compute-augment-picker-rect
 make-augment-picker)

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

(define (compute-augment-picker-rect screen::Screen)
  (let* ((faction-rect (compute-faction-picker-rect screen))
         (screen-width (*:getWidth screen))
         (screen-height (*:getHeight screen))
         (picker-width (get-width faction-rect))
         (picker-left (- screen-width (+ picker-width 16)))
         (picker-top (get-top faction-rect))
         (picker-height (get-height faction-rect)))
    (make-rectangle picker-left picker-top picker-width picker-height)))

(define (make-augment-picker state::FabricClientState screen::Screen)
  (let* ((state::CreateCharacterState (as CreateCharacterState state))
         (rect (compute-augment-picker-rect screen))
         (win (Window screen "AugmentPicker"
                      (Vector2f (get-left rect) (get-top rect))
                      (Vector2f (get-width rect) (get-height rect)))))
    (*:setWindowTitle win "Choose your augment:")
    win))
