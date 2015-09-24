;;;; ***********************************************************************
;;;;
;;;; Name:          view-armor-picker.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the armor picker 
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 compute-armor-picker-rect
 make-armor-picker)

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

(define (compute-armor-picker-rect screen::Screen)
  (let* ((faction-rect (compute-faction-picker-rect screen))
         (screen-width (*:getWidth screen))
         (screen-height (*:getHeight screen))
         (picker-left (get-left faction-rect))
         (picker-top (+ 32 (get-top faction-rect)(get-height faction-rect)))
         (picker-width 256)
         (picker-height (- screen-height picker-top 32)))
    (make-rectangle picker-left picker-top picker-width picker-height)))

(define (make-armor-picker state::FabricClientState screen::Screen)
  (let* ((state::CreateCharacterState (as CreateCharacterState state))
         (rect (compute-armor-picker-rect screen))
         (win (Window screen "ArmorPicker"
                      (Vector2f (get-left rect) (get-top rect))
                      (Vector2f (get-width rect) (get-height rect)))))
    (*:setWindowTitle win "Choose your armor:")
    win))
