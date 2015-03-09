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

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as BitmapFont com.jme3.font.BitmapFont)
(import-as ColorRGBA com.jme3.math.ColorRGBA)
(import-as Label tonegod.gui.controls.text.Label)
(import-as Vector2f com.jme3.math.Vector2f)

;;; ---------------------------------------------------------------------
;;; node nameplates
;;; ---------------------------------------------------------------------

(define (make-node-nameplate screen nodename)
  (let* ((nameplate (Label screen "NodeNameplate" (Vector2f 32 32)(Vector2f 800 40)))
         (Align BitmapFont:Align))
    (*:setText nameplate nodename)
    (*:setTextAlign nameplate Align:Left)
    (*:setFont nameplate "Interface/Fonts/Laconic30.fnt")
    (*:setFontSize nameplate 30)
    (*:setFontColor nameplate ColorRGBA:Green)
    nameplate))

