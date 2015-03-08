;;;; ***********************************************************************
;;;;
;;;; Name:          view-faction-nameplate.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       faction nameplate for the character creator
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 compute-faction-nameplate-origin
 compute-faction-nameplate-size
 make-faction-nameplate)

(require "util-java.scm")
(require "view-faction-palette.scm")
(require "appstate-character-creator.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as BitmapFont com.jme3.font.BitmapFont)
(import-as ColorRGBA com.jme3.math.ColorRGBA)
(import-as Screen tonegod.gui.core.Screen)
(import-as TLabel tonegod.gui.controls.text.Label)
(import-as Vector2f com.jme3.math.Vector2f)
(import-as Window tonegod.gui.controls.windows.Window)

;;; =====================================================================
;;; the faction nameplate
;;; =====================================================================

;;; (compute-faction-nameplate-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the faction nameplate,
;;; taking into accoun the dimensions of the screen

(define (compute-faction-nameplate-origin screen::Screen)
  (let ((faction-palette-size::Vector2f (compute-faction-palette-size screen))
        (height (*:getHeight screen)))
    (Vector2f (+ 32 (*:getX faction-palette-size)) 8)))


;;; (compute-faction-nameplate-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the faction nameplate,
;;; taking into account the dimensions of the screen

(define (compute-faction-nameplate-size screen::Screen)
  (let ((width (*:getWidth screen)))
    (Vector2f 280 40)))


;;; (make-faction-nameplate screen::Screen)
;;; ---------------------------------------------------------------------
;;; constructs and returns a new TLabel object for use as the faction
;;; nameplate

(define (make-faction-nameplate screen::Screen)
  (let ((label (TLabel screen "FactionNameplate"
                       (compute-faction-nameplate-origin screen)
                       (compute-faction-nameplate-size screen)))
        (Align BitmapFont:Align))
    (*:setText label "")
    (*:setTextAlign label Align:Left)
    (*:setFont label "Interface/Fonts/Laconic30.fnt")
    (*:setFontSize label 30)
    (*:setFontColor label ColorRGBA:Green)
    label))

