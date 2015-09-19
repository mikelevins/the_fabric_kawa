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

(require util-java)
(require util-error)
(require client-class)
(require client-state)
(require state-create-character)
(require model-rect)
(require view-faction-button-group)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as BitmapFont com.jme3.font.BitmapFont)
(import-as Button tonegod.gui.controls.buttons.Button)
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
  (let* ((button-width 96)
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
  (Vector2f 96 96))

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
  (let* ((button-width 96)
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
  (Vector2f 96 96))

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
  (let* ((button-width 96)
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
  (Vector2f 96 96))

;;; (make-rogues-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed rogues faction button

(define (make-rogues-button screen::Screen)
  (RadioButton screen "RoguesButton"
               (compute-rogues-button-origin screen)
               (compute-rogues-button-size screen)))

;;; make-character-picker
;;; ---------------------------------------------------------------------

(define (make-faction-picker state::FabricClientState screen::Screen)
  (let* ((state::CreateCharacterState (as CreateCharacterState state))
         (align BitmapFont:Align)
         (valign BitmapFont:VAlign)
         (rect (compute-faction-picker-rect screen))
         (win (Window screen "FactionPicker"
                      (Vector2f (get-left rect) (get-top rect))
                      (Vector2f (get-width rect) (get-height rect))))
         (faction-group (FactionButtonGroup screen "FactionGroup"))
         (caretakers-button::Button (make-caretaker-button screen))
         (rogues-button::Button (make-rogues-button screen))
         (abjurers-button::Button (make-abjurers-button screen)))
    (*:setWindowTitle win "Choose a faction:")
    (set! faction-group:app-state state)
    ;; caretaker button
    (*:setButtonIcon caretakers-button 96 96 "Interface/caretakers-icon96.png")
    (*:setButtonPressedInfo caretakers-button "Interface/caretakers-iconlit96.png" (ColorRGBA 0.0 1.0 0.0 1.0))
    (*:setText caretakers-button "Caretakers")
    (*:setTextAlign caretakers-button align:Center)
    (*:setTextVAlign caretakers-button valign:Bottom)
    (*:setFontSize caretakers-button 20)
    (set! state:caretakers-button caretakers-button)
    (*:addButton faction-group caretakers-button)
    (*:addChild win caretakers-button)
    ;; abjurers button
    (*:setButtonIcon abjurers-button 96 96 "Interface/abjurers-icon96.png")
    (*:setButtonPressedInfo abjurers-button "Interface/abjurers-iconlit96.png" (ColorRGBA 0.0 1.0 0.0 1.0))
    (*:setText abjurers-button "Abjurers")
    (*:setTextAlign abjurers-button align:Center)
    (*:setTextVAlign abjurers-button valign:Bottom)
    (*:setFontSize abjurers-button 20)
    (set! state:abjurers-button abjurers-button)
    (*:addButton faction-group abjurers-button)
    (*:addChild win abjurers-button)
    ;; rogues button
    (*:setButtonIcon rogues-button 96 96 "Interface/rogues-icon96.png")
    (*:setButtonPressedInfo rogues-button "Interface/rogues-iconlit96.png" (ColorRGBA 0.0 1.0 0.0 1.0))
    (*:setText rogues-button "Rogues")
    (*:setTextAlign rogues-button align:Center)
    (*:setTextVAlign rogues-button valign:Bottom)
    (*:setFontSize rogues-button 20)
    (set! state:rogues-button rogues-button)
    (*:addButton faction-group rogues-button)
    (*:addChild win rogues-button)
    win))
