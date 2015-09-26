;;;; ***********************************************************************
;;;;
;;;; Name:          view-action-bar.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the action bar 
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 compute-action-bar-rect
 make-action-bar)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require client)
(require state)
(require state-play)
(require model-rect)
(require view-faction-picker)
(require view-weapon-button-group)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class com.jme3.font BitmapFont))
(import (class com.jme3.math ColorRGBA Vector2f))
(import (class tonegod.gui.controls.buttons RadioButton))
(import (class tonegod.gui.controls.windows Panel))
(import (class tonegod.gui.core Screen))

;;; ---------------------------------------------------------------------
;;; 
;;; ---------------------------------------------------------------------

(define (compute-action-bar-rect screen::Screen)
  (let* ((screen-width (*:getWidth screen))
         (screen-height (*:getHeight screen))
         (bar-width 1024)
         (bar-height 96)
         (bar-left (- (/ screen-width 2)(/ bar-width 2)))
         (bar-top (- screen-height 16 bar-height)))
    (make-rectangle bar-left bar-top bar-width bar-height)))

(define (make-action-bar state::FabricClientState screen::Screen)
  (let* ((state::PlayState (as PlayState state))
         (rect (compute-action-bar-rect screen))
         (win (Panel screen "ActionBar"
                      (Vector2f (get-left rect)(get-top rect))
                      (Vector2f (get-width rect)(get-height rect)))))
    win))
