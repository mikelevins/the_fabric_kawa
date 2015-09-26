;;;; ***********************************************************************
;;;;
;;;; Name:          view-armor-picker.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the armor picker 
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 compute-armor-picker-rect
 make-armor-picker)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require client)
(require state)
(require state-create-character)
(require model-rect)
(require view-armor-button-group)
(require view-faction-picker)
(require view-weapon-picker)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class com.jme3.font BitmapFont))
(import (class com.jme3.math ColorRGBA Vector2f))
(import (class tonegod.gui.controls.buttons RadioButton))
(import (class tonegod.gui.controls.windows Window))
(import (class tonegod.gui.core Screen))

;;; ---------------------------------------------------------------------
;;; 
;;; ---------------------------------------------------------------------

;;; (compute-absorb-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the absorb faction
;;; button

(define (compute-absorb-button-origin screen::Screen)
  (let* ((button-width 96)
         (button-height 96)
         (rect (compute-weapon-picker-rect screen))
         (palette-width (get-width rect))
         (palette-height (get-height rect))
         (x (- (/ palette-width 2.0)
               (/ button-width 2.0)))
         (y (+ 16 (- (* 1 (/ palette-height 9.0))
                     (/ button-height 2.0)))))
    (Vector2f x y)))

;;; (compute-absorb-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the absorb faction
;;; button

(define (compute-absorb-button-size screen::Screen)
  (Vector2f 96 96))

;;; (make-absorb-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed absorb faction button

(define (make-absorb-button screen::Screen)
  (RadioButton screen "AbsorbButton"
               (compute-absorb-button-origin screen)
               (compute-absorb-button-size screen)))


;;; (compute-regenerate-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the regenerate faction
;;; button

(define (compute-regenerate-button-origin screen::Screen)
  (let* ((button-width 96)
         (button-height 96)
         (rect (compute-weapon-picker-rect screen))
         (palette-width (get-width rect))
         (palette-height (get-height rect))
         (x (- (/ palette-width 2.0)
               (/ button-width 2.0)))
         (y (+ 16 (- (* 3 (/ palette-height 9.0))
                     (/ button-height 2.0)))))
    (Vector2f x y)))

;;; (compute-regenerate-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the regenerate faction
;;; button

(define (compute-regenerate-button-size screen::Screen)
  (Vector2f 96 96))

;;; (make-regenerate-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed regenerate faction button

(define (make-regenerate-button screen::Screen)
  (RadioButton screen "RegenerateButton"
               (compute-regenerate-button-origin screen)
               (compute-regenerate-button-size screen)))


;;; (compute-power-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the power faction
;;; button

(define (compute-power-button-origin screen::Screen)
  (let* ((button-width 96)
         (button-height 96)
         (rect (compute-weapon-picker-rect screen))
         (palette-width (get-width rect))
         (palette-height (get-height rect))
         (x (- (/ palette-width 2.0)
               (/ button-width 2.0)))
         (y (+ 16 (- (* 5 (/ palette-height 9.0))
                     (/ button-height 2.0)))))
    (Vector2f x y)))

;;; (compute-power-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the power faction
;;; button

(define (compute-power-button-size screen::Screen)
  (Vector2f 96 96))

;;; (make-power-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed power faction button

(define (make-power-button screen::Screen)
  (RadioButton screen "PowerButton"
               (compute-power-button-origin screen)
               (compute-power-button-size screen)))


;;; (compute-energy-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the energy faction
;;; button

(define (compute-energy-button-origin screen::Screen)
  (let* ((button-width 96)
         (button-height 96)
         (rect (compute-weapon-picker-rect screen))
         (palette-width (get-width rect))
         (palette-height (get-height rect))
         (x (- (/ palette-width 2.0)
               (/ button-width 2.0)))
         (y (+ 16 (- (* 7 (/ palette-height 9.0))
                     (/ button-height 2.0)))))
    (Vector2f x y)))

;;; (compute-energy-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the energy faction
;;; button

(define (compute-energy-button-size screen::Screen)
  (Vector2f 96 96))

;;; (make-energy-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed energy faction button

(define (make-energy-button screen::Screen)
  (RadioButton screen "EnergyButton"
               (compute-energy-button-origin screen)
               (compute-energy-button-size screen)))

(define (compute-armor-picker-rect screen::Screen)
  (let* ((faction-rect (compute-faction-picker-rect screen))
         (screen-width (*:getWidth screen))
         (screen-height (*:getHeight screen))
         (picker-left (get-left faction-rect))
         (picker-top (+ 16 (get-top faction-rect)(get-height faction-rect)))
         (picker-width 256)
         (picker-height (- screen-height picker-top 16)))
    (make-rectangle picker-left picker-top picker-width picker-height)))

(define (make-armor-picker state::FabricClientState screen::Screen)
  (let* ((state::CreateCharacterState (as CreateCharacterState state))
         (rect (compute-armor-picker-rect screen))
         (align BitmapFont:Align)
         (valign BitmapFont:VAlign)
         (win (Window screen "ArmorPicker"
                      (Vector2f (get-left rect)(get-top rect))
                      (Vector2f (get-width rect)(get-height rect))))
         (armor-group::ArmorButtonGroup (ArmorButtonGroup screen "ArmorGroup"))
         (absorb-button (make-absorb-button screen))
         (regenerate-button (make-regenerate-button screen))
         (power-button (make-power-button screen))
         (energy-button (make-energy-button screen)))
    (*:setWindowTitle win "Choose your armor:")
    (set! armor-group:state state)
    ;; absorb button
    (*:setButtonIcon absorb-button 96 96 "Interface/absorb-armor-icon96.png")
    (*:setButtonPressedInfo absorb-button "Interface/absorb-armor-icon96.png"
                            (ColorRGBA 0.0 1.0 0.0 1.0))
    (*:setText absorb-button "Absorb")
    (*:setTextAlign absorb-button align:Center)
    (*:setTextVAlign absorb-button valign:Bottom)
    (*:setFontSize absorb-button 20)
    (*:addButton armor-group absorb-button)
    (*:addChild win absorb-button)
    ;; regenerate button
    (*:setButtonIcon regenerate-button 96 96 "Interface/regen-armor-icon96.png")
    (*:setButtonPressedInfo regenerate-button "Interface/regen-armor-icon96.png"
                            (ColorRGBA 0.0 1.0 0.0 1.0))
    (*:setText regenerate-button "Regenerate")
    (*:setTextAlign regenerate-button align:Center)
    (*:setTextVAlign regenerate-button valign:Bottom)
    (*:setFontSize regenerate-button 20)
    (*:addButton armor-group regenerate-button)
    (*:addChild win regenerate-button)
    ;; power button
    (*:setButtonIcon power-button 96 96 "Interface/power-armor-icon96.png")
    (*:setButtonPressedInfo power-button "Interface/power-armor-icon96.png"
                            (ColorRGBA 0.0 1.0 0.0 1.0))
    (*:setText power-button "Power")
    (*:setTextAlign power-button align:Center)
    (*:setTextVAlign power-button valign:Bottom)
    (*:setFontSize power-button 20)
    (*:addButton armor-group power-button)
    (*:addChild win power-button)
    ;; energy button
    (*:setButtonIcon energy-button 96 96 "Interface/energy-armor-icon96.png")
    (*:setButtonPressedInfo energy-button "Interface/energy-armor-icon96.png"
                            (ColorRGBA 0.0 1.0 0.0 1.0))
    (*:setText energy-button "Energy")
    (*:setTextAlign energy-button align:Center)
    (*:setTextVAlign energy-button valign:Bottom)
    (*:setFontSize energy-button 20)
    (*:addButton armor-group energy-button)
    (*:addChild win energy-button)
    win))
