;;;; ***********************************************************************
;;;;
;;;; Name:          view-character-nameplate.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       character nameplate for the character creator
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 compute-character-nameplate-origin
 compute-character-nameplate-size
 make-character-nameplate)

(require "util-java.scm")
(require "view-weapons-palette.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as BitmapFont com.jme3.font.BitmapFont)
(import-as Button tonegod.gui.controls.buttons.Button)
(import-as ColorRGBA com.jme3.math.ColorRGBA)
(import-as RadioButtonGroup tonegod.gui.controls.buttons.RadioButtonGroup)
(import-as Screen tonegod.gui.core.Screen)
(import-as TLabel tonegod.gui.controls.text.Label)
(import-as Vector2f com.jme3.math.Vector2f)

;;; =====================================================================
;;; the character nameplate
;;; =====================================================================

;;; (compute-character-nameplate-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the character nameplate,
;;; taking into accoun the dimensions of the screen

(define (compute-character-nameplate-origin screen::Screen)
  (let ((weapons-palette-size::Vector2f (compute-weapons-palette-size screen))
        (weapons-palette-origin::Vector2f (compute-weapons-palette-origin screen))
        (nameplate-size::Vector2f (compute-character-nameplate-size screen)))
    (Vector2f (+ 32 (*:getX weapons-palette-size))
              (- (+ (*:getY weapons-palette-origin)
                    (*:getY weapons-palette-size))
                 (*:getY nameplate-size)
                 8))))


;;; (compute-character-nameplate-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the character nameplate,
;;; taking into accoun the dimensions of the screen

(define (compute-character-nameplate-size screen::Screen)
  (let ((width (*:getWidth screen)))
    (Vector2f 960 40)))


;;; (make-character-nameplate screen::Screen)
;;; ---------------------------------------------------------------------
;;; constructs and returns a new TLabel object for use as the character
;;; nameplate

(define (make-character-nameplate screen::Screen)
  (let ((label (TLabel screen "CharacterNameplate"
                       (compute-character-nameplate-origin screen)
                       (compute-character-nameplate-size screen)))
        (Align BitmapFont:Align))
    (*:setText label "")
    (*:setTextAlign label Align:Left)
    (*:setFont label "Interface/Fonts/Laconic30.fnt")
    (*:setFontSize label 30)
    (*:setFontColor label ColorRGBA:Green)
    label))


