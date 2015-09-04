;;;; ***********************************************************************
;;;;
;;;; Name:          view-pickarmor.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the armor picker 
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 make-armor-picker)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "util-error.scm")
(require "client-class.scm")
(require "view-faction-picker.scm")
(require "client-state.scm")
(require "state-create-character.scm")
(require "model-rect.scm")
(require "view-armors-button-group.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as BitmapFont com.jme3.font.BitmapFont)
(import-as ColorRGBA com.jme3.math.ColorRGBA)
(import-as RadioButton tonegod.gui.controls.buttons.RadioButton)
(import-as Screen tonegod.gui.core.Screen)
(import-as Vector2f com.jme3.math.Vector2f)
(import-as Window tonegod.gui.controls.windows.Window)


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

;;; make-armor-picker
;;; ---------------------------------------------------------------------

(define (make-armor-picker state::FabricClientState screen::Screen)
  (let* ((rect (compute-armor-picker-rect screen))
         (align BitmapFont:Align)
         (valign BitmapFont:VAlign)
         (win (Window screen "ArmorPicker"
                      (Vector2f (get-left rect)(get-top rect))
                      (Vector2f (get-width rect)(get-height rect))))
         (armors-group::ArmorsButtonGroup (ArmorsButtonGroup screen "ArmorsGroup"))
         (absorb-button (make-absorb-button screen))
         (regenerate-button (make-regenerate-button screen))
         (power-button (make-power-button screen))
         (energy-button (make-energy-button screen)))
    (*:setWindowTitle win "Choose an armor:")
    (*:setAppState armors-group state)
    ;; absorb button
    (*:setButtonIcon absorb-button 96 96 "Interface/absorb-armor-icon96.png")
    (*:setButtonPressedInfo absorb-button "Interface/absorb-armor-icon96.png"
                            (ColorRGBA 0.0 1.0 0.0 1.0))
    (*:setText absorb-button "Absorb")
    (*:setTextAlign absorb-button align:Center)
    (*:setTextVAlign absorb-button valign:Bottom)
    (*:setFontSize absorb-button 20)
    (*:addButton armors-group absorb-button)
    (*:addChild win absorb-button)
    ;; regenerate button
    (*:setButtonIcon regenerate-button 96 96 "Interface/regen-armor-icon96.png")
    (*:setButtonPressedInfo regenerate-button "Interface/regen-armor-icon96.png"
                            (ColorRGBA 0.0 1.0 0.0 1.0))
    (*:setText regenerate-button "Regenerate")
    (*:setTextAlign regenerate-button align:Center)
    (*:setTextVAlign regenerate-button valign:Bottom)
    (*:setFontSize regenerate-button 20)
    (*:addButton armors-group regenerate-button)
    (*:addChild win regenerate-button)
    ;; power button
    (*:setButtonIcon power-button 96 96 "Interface/power-armor-icon96.png")
    (*:setButtonPressedInfo power-button "Interface/power-armor-icon96.png"
                            (ColorRGBA 0.0 1.0 0.0 1.0))
    (*:setText power-button "Power")
    (*:setTextAlign power-button align:Center)
    (*:setTextVAlign power-button valign:Bottom)
    (*:setFontSize power-button 20)
    (*:addButton armors-group power-button)
    (*:addChild win power-button)
    ;; energy button
    (*:setButtonIcon energy-button 96 96 "Interface/energy-armor-icon96.png")
    (*:setButtonPressedInfo energy-button "Interface/energy-armor-icon96.png"
                            (ColorRGBA 0.0 1.0 0.0 1.0))
    (*:setText energy-button "Energy")
    (*:setTextAlign energy-button align:Center)
    (*:setTextVAlign energy-button valign:Bottom)
    (*:setFontSize energy-button 20)
    (*:addButton armors-group energy-button)
    (*:addChild win energy-button)
    win))
