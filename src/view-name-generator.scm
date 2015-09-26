;;;; ***********************************************************************
;;;;
;;;; Name:          view-name-generator.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the name-generator UI 
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 compute-name-generator-rect
 make-name-generator)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require state)
(require state-create-character)
(require model-rect)
(require view-faction-picker)
(require view-armor-picker)
(require view-random-name-button)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class com.jme3.font BitmapFont BitmapText))
(import (class com.jme3.math ColorRGBA Vector2f))
(import (class tonegod.gui.controls.buttons Button))
(import (class tonegod.gui.controls.lists SelectList))
(import (class tonegod.gui.controls.text Label))
(import (class tonegod.gui.controls.windows Window))
(import (class tonegod.gui.core Screen))

;;; ---------------------------------------------------------------------
;;; 
;;; ---------------------------------------------------------------------

(define (compute-name-generator-rect screen::Screen)
  (let* ((faction-rect (compute-faction-picker-rect screen))
         (armor-rect (compute-armor-picker-rect screen))
         (screen-width (*:getWidth screen))
         (screen-height (*:getHeight screen))
         (picker-width 200)
         (picker-left (+ 16 (get-width armor-rect) 16))
         (picker-height 100)
         (picker-top (- screen-height 16 picker-height)))
    (make-rectangle picker-left picker-top picker-width picker-height)))

(define (make-name-generator state::FabricClientState screen::Screen)
  (let* ((state::CreateCharacterState (as CreateCharacterState state))
         (rect (compute-name-generator-rect screen))
         (win (Window screen "NameGenerator"
                      (Vector2f (get-left rect) (get-top rect))
                      (Vector2f (get-width rect) (get-height rect))))
         (btn::Button (make-random-name-button screen state))
         (Align BitmapFont:Align)
         (position (Vector2f 10 48))
         (size (Vector2f 600 24.0))
         (f (Label screen "name field" position size))
         (btn::Button (make-random-name-button screen state)))
    (*:setText f "")
    (*:setTextAlign f Align:Left)
    (*:setFont f "Interface/Fonts/Laconic24.fnt")
    (*:setFontSize f 24)
    (*:setFontColor f ColorRGBA:Green)
    (*:addChild win f)
    (*:addChild win btn)
    (*:setWindowTitle win "Generate a name:")
    win))
