;;;; ***********************************************************************
;;;;
;;;; Name:          view-location-nameplate.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the location nameplate for the character creator
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 make-location-nameplate)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require state-create-character)
(require model-rect)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class com.jme3.font BitmapFont BitmapText))
(import (class com.jme3.math ColorRGBA))
(import (class com.jme3.math Vector2f))
(import (class tonegod.gui.controls.text Label))
(import (class tonegod.gui.core Screen))

;;; ---------------------------------------------------------------------
;;; location nameplate
;;; ---------------------------------------------------------------------

(define (make-location-nameplate screen::Screen)
  (let* ((screen-height (*:getHeight screen))
         (Align BitmapFont:Align)
         (nameplate-left 32)
         (nameplate-top (- screen-height 64))
         (nameplate-width 600)
         (nameplate-height 64)
         (location-nameplate::Label (Label screen "LocationNameplate"
                                           (Vector2f nameplate-left nameplate-top)
                                           (Vector2f nameplate-width nameplate-height))))
    (*:setText location-nameplate "Location: ")
    (*:setTextAlign location-nameplate Align:Left)
    (*:setFont location-nameplate "Interface/Fonts/Laconic30.fnt")
    (*:setFontSize location-nameplate 30)
    (*:setFontColor location-nameplate ColorRGBA:Green)
    location-nameplate))

