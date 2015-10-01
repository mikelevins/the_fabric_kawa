;;;; ***********************************************************************
;;;;
;;;; Name:          view-name-generator.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the random name generator 
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 make-name-generator)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require 'list-lib)
(require util-error)
(require client-class)
(require model-character)
(require model-rect)
(require data-names)
(require view-random-name-button)
(require state-create-character)

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

;;; make-name-picker
;;; ---------------------------------------------------------------------

(define (make-name-generator screen::Screen state::CreateCharacterState fname::FabricName)
  (let* ((Align BitmapFont:Align)
         (rect (compute-name-generator-rect screen))
         (position (Vector2f 10 48))
         (size (Vector2f 600 24.0))
         (f (Label screen "name field" position size))
         (btn::Button (make-random-name-button screen state))
         (win (Window screen "NameGenerator"
                      (Vector2f (get-left rect) (get-top rect))
                      (Vector2f (get-width rect) (get-height rect)))))
    (*:setText f "")
    (*:setTextAlign f Align:Left)
    (*:setFont f "Interface/Fonts/Laconic24.fnt")
    (*:setFontSize f 24)
    (*:setFontColor f ColorRGBA:Green)
    (*:addChild win f)
    (*:addChild win btn)
    (*:setWindowTitle win "Generate a Fabric name:")
    win))
