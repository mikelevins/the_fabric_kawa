;;;; ***********************************************************************
;;;;
;;;; Name:          view-character-pick-acceptor.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the acceptor for picking a character
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 make-character-pick-acceptor)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require client)
(require state)
(require state-pick-character)
(require model-rect)
(require view-armor-picker)
(require view-faction-picker)
(require view-character-picker)
(require view-accept-character-pick-button)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class com.jme3.math ColorRGBA Vector2f))
(import (class tonegod.gui.controls.windows Window))
(import (class tonegod.gui.core Screen))

;;; ---------------------------------------------------------------------
;;; character pick acceptor
;;; ---------------------------------------------------------------------

(define (compute-character-pick-acceptor-rect screen::Screen)
  (let* ((faction-rect (compute-faction-picker-rect screen))
         (armor-rect (compute-armor-picker-rect screen))
         (screen-width (*:getWidth screen))
         (screen-height (*:getHeight screen))
         (acceptor-width 200)
         (acceptor-left (- screen-width 48 (get-width armor-rect) acceptor-width))
         (acceptor-height 100)
         (acceptor-top (- screen-height 16 acceptor-height)))
    (make-rectangle acceptor-left acceptor-top acceptor-width acceptor-height)))

(define (make-character-pick-acceptor state::PickCharacterState screen)
  (let* ((rect (compute-character-pick-acceptor-rect screen))
         (win::Window (Window screen "CharacterPickAcceptor"
                              (Vector2f (get-left rect) (get-top rect))
                              (Vector2f (get-width rect) (get-height rect))))
         (save-button (make-accept-character-pick-button screen state)))
    (*:setWindowTitle win "Choose this character:")
    (*:addChild win save-button)
    win))

