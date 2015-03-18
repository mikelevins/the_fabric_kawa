;;;; ***********************************************************************
;;;;
;;;; Name:          view-pickfaction.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the faction picker 
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 make-faction-picker)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "util-error.scm")
(require "syntax-classes.scm")
(require "client-main.scm")
(require "gamestates.scm")
(require "gamestates-createchar.scm")
(require "model-rect.scm")
(require "view-faction-button-group.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as BitmapFont com.jme3.font.BitmapFont)
(import-as ColorRGBA com.jme3.math.ColorRGBA)
(import-as RadioButton tonegod.gui.controls.buttons.RadioButton)
(import-as Screen tonegod.gui.core.Screen)
(import-as Vector2f com.jme3.math.Vector2f)
(import-as Window tonegod.gui.controls.windows.Window)


;;; (compute-caretaker-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the caretakers faction
;;; button

(define (compute-caretaker-button-origin screen::Screen)
  (let* ((button-width 128)
         (button-height 96)
         (rect (compute-faction-picker-rect screen))
         (palette-width (get-width rect))
         (palette-height (get-height rect))
         (x (- (/ palette-width 6.0)
               (/ button-width 2.0)))
         (y (- (/ palette-height 2.0)
               (/ button-height 2.0))))
    (Vector2f x y)))

;;; (compute-caretaker-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the caretakers faction
;;; button

(define (compute-caretaker-button-size screen::Screen)
  (Vector2f 128 96))

;;; (make-caretaker-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed caretakers faction button

(define (make-caretaker-button screen::Screen)
  (RadioButton screen "CaretakerButton"
               (compute-caretaker-button-origin screen)
               (compute-caretaker-button-size screen)))


;;; (compute-abjurers-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the abjurers faction
;;; button

(define (compute-abjurers-button-origin screen::Screen)
  (let* ((button-width 128)
         (button-height 96)
         (rect (compute-faction-picker-rect screen))
         (palette-width (get-width rect))
         (palette-height (get-height rect))
         (x (- (* 3 (/ palette-width 6.0))
               (/ button-width 2.0)))
         (y (- (/ palette-height 2.0)
               (/ button-height 2.0))))
    (Vector2f x y)))

;;; (compute-abjurers-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the abjurers faction
;;; button

(define (compute-abjurers-button-size screen::Screen)
  (Vector2f 128 96))

;;; (make-abjurers-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed abjurers faction button

(define (make-abjurers-button screen::Screen)
  (RadioButton screen "AbjurersButton"
               (compute-abjurers-button-origin screen)
               (compute-abjurers-button-size screen)))


;;; (compute-rogues-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the rogues faction
;;; button

(define (compute-rogues-button-origin screen::Screen)
  (let* ((button-width 128)
         (button-height 96)
         (rect (compute-faction-picker-rect screen))
         (palette-width (get-width rect))
         (palette-height (get-height rect))
         (x (- (* 5 (/ palette-width 6.0))
               (/ button-width 2.0)))
         (y (- (/ palette-height 2.0)
               (/ button-height 2.0))))
    (Vector2f x y)))

;;; (compute-rogues-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the rogues faction
;;; button

(define (compute-rogues-button-size screen::Screen)
  (Vector2f 128 96))

;;; (make-rogues-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed rogues faction button

(define (make-rogues-button screen::Screen)
  (RadioButton screen "RoguesButton"
               (compute-rogues-button-origin screen)
               (compute-rogues-button-size screen)))

;;; make-character-picker
;;; ---------------------------------------------------------------------

(define (make-faction-picker state::FabricGameState screen::Screen)
  (let* ((align BitmapFont:Align)
         (valign BitmapFont:VAlign)
         (rect (compute-faction-picker-rect screen))
         (win (Window screen "FactionPicker"
                      (Vector2f (get-left rect) (get-top rect))
                      (Vector2f (get-width rect) (get-height rect))))
         (faction-group (FactionButtonGroup screen "FactionGroup"))
         (caretaker-button (make-caretaker-button screen))
         (abjurers-button (make-abjurers-button screen))
         (rogues-button (make-rogues-button screen)))
    (*:setWindowTitle win "Choose a faction:")
    (*:setAppState faction-group state)
    ;; caretaker button
    (*:setButtonIcon caretaker-button 128 128 "Interface/caretakers-icon128.png")
    (*:setButtonPressedInfo caretaker-button "Interface/caretakers-icon128.png"
                            (ColorRGBA 0.0 1.0 0.0 1.0))
    (*:setText caretaker-button "Caretakers")
    (*:setTextAlign caretaker-button align:Center)
    (*:setTextVAlign caretaker-button valign:Bottom)
    (*:setFontSize caretaker-button 20)
    (*:addButton faction-group caretaker-button)
    (*:addChild win caretaker-button)
    ;; abjurers button
    (*:setButtonIcon abjurers-button 128 128 "Interface/abjurers-icon128.png")
    (*:setButtonPressedInfo abjurers-button "Interface/abjurers-icon128.png"
                            (ColorRGBA 0.0 1.0 0.0 1.0))
    (*:setText abjurers-button "Abjurers")
    (*:setTextAlign abjurers-button align:Center)
    (*:setTextVAlign abjurers-button valign:Bottom)
    (*:setFontSize abjurers-button 20)
    (*:addButton faction-group abjurers-button)
    (*:addChild win abjurers-button)
    ;; rogues button
    (*:setButtonIcon rogues-button 128 128 "Interface/rogues-icon128.png")
    (*:setButtonPressedInfo rogues-button "Interface/rogues-icon128.png"
                            (ColorRGBA 0.0 1.0 0.0 1.0))
    (*:setText rogues-button "Rogues")
    (*:setTextAlign rogues-button align:Center)
    (*:setTextVAlign rogues-button valign:Bottom)
    (*:setFontSize rogues-button 20)
    (*:addButton faction-group rogues-button)
    (*:addChild win rogues-button)
    win))
