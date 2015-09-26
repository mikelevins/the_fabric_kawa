;;;; ***********************************************************************
;;;;
;;;; Name:          view-character-acceptor.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the character-acceptor UI 
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 compute-character-acceptor-rect
 make-character-acceptor)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require state)
(require state-create-character)
(require model-rect)
(require view-faction-picker)
(require view-armor-picker)
(require view-save-character-button)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class com.jme3.math ColorRGBA Vector2f))
(import (class tonegod.gui.controls.windows Window))
(import (class tonegod.gui.core Screen))

;;; ---------------------------------------------------------------------
;;; 
;;; ---------------------------------------------------------------------

(define (compute-character-acceptor-rect screen::Screen)
  (let* ((faction-rect (compute-faction-picker-rect screen))
         (armor-rect (compute-armor-picker-rect screen))
         (screen-width (*:getWidth screen))
         (screen-height (*:getHeight screen))
         (acceptor-width 200)
         (acceptor-left (- screen-width 48 (get-width armor-rect) acceptor-width))
         (acceptor-height 100)
         (acceptor-top (- screen-height 16 acceptor-height)))
    (make-rectangle acceptor-left acceptor-top acceptor-width acceptor-height)))

(define (make-character-acceptor state::FabricClientState screen::Screen)
  (let* ((state::CreateCharacterState (as CreateCharacterState state))
         (rect (compute-character-acceptor-rect screen))
         (win (Window screen "CharacterAcceptor"
                      (Vector2f (get-left rect) (get-top rect))
                      (Vector2f (get-width rect) (get-height rect))))
         (save-button (make-save-character-button screen state)))
    (*:setWindowTitle win "Save this character:")
    (*:addChild win save-button)
    win))
