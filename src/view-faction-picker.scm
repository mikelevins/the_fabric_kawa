;;;; ***********************************************************************
;;;;
;;;; Name:          view-pickfaction.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the faction picker 
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 compute-faction-picker-rect
 make-faction-picker)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require state)
(require state-create-character)
(require model-rect)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class com.jme3.math ColorRGBA Vector2f))
(import (class tonegod.gui.controls.windows Window))
(import (class tonegod.gui.core Screen))

;;; ---------------------------------------------------------------------
;;; 
;;; ---------------------------------------------------------------------

(define (compute-faction-picker-rect screen::Screen)
  (make-rectangle 8 8 640 200))

(define (make-faction-picker state::FabricClientState screen::Screen)
  (let* ((state::CreateCharacterState (as CreateCharacterState state))
         (rect (compute-faction-picker-rect screen))
         (win (Window screen "FactionPicker"
                      (Vector2f (get-left rect) (get-top rect))
                      (Vector2f (get-width rect) (get-height rect)))))
    (*:setWindowTitle win "Choose a faction:")
    win))
