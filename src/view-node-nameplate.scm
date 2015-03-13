;;;; ***********************************************************************
;;;;
;;;; Name:          view-node-nameplate.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       displaying the Fabric node name in play screens
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 make-node-nameplate)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "gamestates-play.scm")
(require "model-rect.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as BitmapFont com.jme3.font.BitmapFont)
(import-as ColorRGBA com.jme3.math.ColorRGBA)
(import-as Label tonegod.gui.controls.text.Label)
(import-as Screen tonegod.gui.core.Screen)
(import-as Vector2f com.jme3.math.Vector2f)

;;; ---------------------------------------------------------------------
;;; node nameplates
;;; ---------------------------------------------------------------------

(define (make-node-nameplate screen::Screen nodename)
  (let* ((rect (compute-node-nameplate-rect screen))
         (screen-width (*:getWidth screen))
         (nameplate (Label screen "NodeNameplate"
                           (Vector2f (get-left rect)(get-top rect))
                           (Vector2f (get-width rect)(get-height rect))))
         (Align BitmapFont:Align))
    (*:setText nameplate nodename)
    (*:setTextAlign nameplate Align:Right)
    (*:setFont nameplate "Interface/Fonts/Laconic30.fnt")
    (*:setFontSize nameplate 30)
    (*:setFontColor nameplate ColorRGBA:Green)
    nameplate))

