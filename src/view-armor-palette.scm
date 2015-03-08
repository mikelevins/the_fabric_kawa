;;;; ***********************************************************************
;;;;
;;;; Name:          view-armor-palette.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the armor palette for the character creator
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 ArmorButtonGroup
 compute-armor-palette-origin
 compute-armor-palette-size
 make-absorb-armor-button
 make-armor-palette
 make-energy-armor-button
 make-power-armor-button
 make-regen-armor-button)

(require "util-java.scm")
(require "syntax-classes.scm")
(require "view-weapons-palette.scm")
(require "appstate-character-creator.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as Button tonegod.gui.controls.buttons.Button)
(import-as ColorRGBA com.jme3.math.ColorRGBA)
(import-as RadioButton tonegod.gui.controls.buttons.RadioButton)
(import-as RadioButtonGroup tonegod.gui.controls.buttons.RadioButtonGroup)
(import-as Screen tonegod.gui.core.Screen)
(import-as Vector2f com.jme3.math.Vector2f)
(import-as Window tonegod.gui.controls.windows.Window)

;;; =====================================================================
;;; the armor palette
;;; =====================================================================

;;; CLASS ArmorButtonGroup
;;; ---------------------------------------------------------------------
;;; a RadioButtonGroup subclass used to present armor options to
;;; players

(defclass ArmorButtonGroup (RadioButtonGroup)
  (slots:
   (app-state init-form: #!null getter: getAppState setter: setAppState))
  (methods:
   ((*init* screen::Screen uid::String)
    (invoke-special RadioButtonGroup (this) '*init* screen uid))
   ((onSelect index::int value::Button)
    (let ((button-id (*:getUID value))
          (state::CharacterCreatorAppState app-state))
      (cond
       ((equal? "AbsorbArmorButton" button-id)(set-current-armor state 'absorb-armor))
       ((equal? "RegenArmorButton" button-id)(set-current-armor state 'regenerate-armor))
       ((equal? "PowerArmorButton" button-id)(set-current-armor state 'power-armor))
       ((equal? "EnergyArmorButton" button-id)(set-current-armor state 'energy-armor))
       (else (format #t "~%Unknown armor selected")))))))



;;; (compute-armor-palette-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the armor palette, taking
;;; into account the dimensions of the screen

(define (compute-armor-palette-origin screen::Screen)
  (let ((weapons-palette-origin (compute-weapons-palette-origin screen))
        (armor-palette-size (compute-armor-palette-size screen))
        (screen-width (*:getWidth screen)))
    (Vector2f (- screen-width (*:getX armor-palette-size) 8)
              (*:getY weapons-palette-origin))))


;;; (compute-armor-palette-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the armor palette, taking
;;; into account the dimensions of the screen

(define (compute-armor-palette-size screen::Screen)
  (let ((weapons-palette-size (compute-weapons-palette-size screen)))
    (Vector2f (*:getX weapons-palette-size)
              (*:getY weapons-palette-size))))


;;; (make-armor-palette screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-contructed armor palette

(define (make-armor-palette screen::Screen)
  (let ((armor-palette (Window screen "ArmorPalette"
                               (compute-armor-palette-origin screen)
                               (compute-armor-palette-size screen)))
        (align BitmapFont:Align)
        (valign BitmapFont:VAlign)
        (armor-group (ArmorButtonGroup screen "ArmorGroup"))
        (absorb-armor-button (make-absorb-armor-button screen))
        (regen-armor-button (make-regen-armor-button screen))
        (power-armor-button (make-power-armor-button screen))
        (energy-armor-button (make-energy-armor-button screen)))
    (*:setWindowTitle armor-palette "Choose Armor:")
    (*:setAppState armor-group state)
    ;; absorb armor button
    (*:setButtonIcon absorb-armor-button 128 128 "Interface/absorb-armor-icon128.png")
    (*:setButtonPressedInfo absorb-armor-button "Interface/absorb-armor-icon128.png"
                            (ColorRGBA 1.0 1.0 0.0 1.0))
    (*:setText absorb-armor-button "Absorb")
    (*:setTextAlign absorb-armor-button align:Center)
    (*:setTextVAlign absorb-armor-button valign:Bottom)
    (*:setFontSize absorb-armor-button 20)
    (*:addButton armor-group absorb-armor-button)
    (*:addChild armor-palette absorb-armor-button)
    ;; regen armor button
    (*:setButtonIcon regen-armor-button 128 128 "Interface/regen-armor-icon128.png")
    (*:setButtonPressedInfo regen-armor-button "Interface/regen-armor-icon128.png"
                            (ColorRGBA 0.0 1.0 0.0 1.0))
    (*:setText regen-armor-button "Regenerate")
    (*:setTextAlign regen-armor-button align:Center)
    (*:setTextVAlign regen-armor-button valign:Bottom)
    (*:setFontSize regen-armor-button 20)
    (*:addButton armor-group regen-armor-button)
    (*:addChild armor-palette regen-armor-button)
    ;; power armor button
    (*:setButtonIcon power-armor-button 128 128 "Interface/power-armor-icon128.png")
    (*:setButtonPressedInfo power-armor-button "Interface/power-armor-icon128.png"
                            (ColorRGBA 1.0 0.5 0.0 1.0))
    (*:setText power-armor-button "Power")
    (*:setTextAlign power-armor-button align:Center)
    (*:setTextVAlign power-armor-button valign:Bottom)
    (*:setFontSize power-armor-button 20)
    (*:addButton armor-group power-armor-button)
    (*:addChild armor-palette power-armor-button)
    ;; energy armor button
    (*:setButtonIcon energy-armor-button 128 128 "Interface/energy-armor-icon128.png")
    (*:setButtonPressedInfo energy-armor-button "Interface/energy-armor-icon128.png"
                            (ColorRGBA 0.0 6.0 1.0 1.0))
    (*:setText energy-armor-button "Energy")
    (*:setTextAlign energy-armor-button align:Center)
    (*:setTextVAlign energy-armor-button valign:Bottom)
    (*:setFontSize energy-armor-button 20)
    (*:addButton armor-group energy-armor-button)
    (*:addChild armor-palette energy-armor-button)
    armor-palette))

;;; (compute-absorb-armor-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the absorb armor
;;; button

(define (compute-absorb-armor-button-origin screen::Screen)
  (let ((armor-palette-size (compute-armor-palette-size screen))
        (armor-button-size (compute-absorb-armor-button-size screen)))
    (Vector2f (- (/ (*:getX armor-palette-size) 2.0)
                 (/ (*:getX armor-button-size) 2.0))
              (- (/ (* 1 (*:getY armor-palette-size)) 8.0)
                 (/ (*:getY armor-button-size) 2.0)))))


;;; (compute-absorb-armor-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the absorb armor
;;; button


(define (compute-absorb-armor-button-size screen::Screen)
  (Vector2f 128 96))

;;; (make-absorb-armor-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed absorb armor button

(define (make-absorb-armor-button screen::Screen)
  (RadioButton screen "AbsorbArmorButton"
               (compute-absorb-armor-button-origin screen)
               (compute-absorb-armor-button-size screen)))


;;; (compute-regen-armor-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the absorb armor
;;; button


(define (compute-regen-armor-button-origin screen::Screen)
  (let ((armor-palette-size (compute-armor-palette-size screen))
        (armor-button-size (compute-absorb-armor-button-size screen)))
    (Vector2f (- (/ (*:getX armor-palette-size) 2.0)
                 (/ (*:getX armor-button-size) 2.0))
              (- (/ (* 3 (*:getY armor-palette-size)) 8.0)
                 (/ (*:getY armor-button-size) 2.0)))))


;;; (compute-regen-armor-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the absorb armor
;;; button


(define (compute-regen-armor-button-size screen::Screen)
  (Vector2f 128 96))

;;; (make-regen-armor-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed absorb armor button

(define (make-regen-armor-button screen::Screen)
  (RadioButton screen "RegenArmorButton"
               (compute-regen-armor-button-origin screen)
               (compute-regen-armor-button-size screen)))


;;; (compute-power-armor-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the absorb armor
;;; button


(define (compute-power-armor-button-origin screen::Screen)
  (let ((armor-palette-size (compute-armor-palette-size screen))
        (armor-button-size (compute-absorb-armor-button-size screen)))
    (Vector2f (- (/ (*:getX armor-palette-size) 2.0)
                 (/ (*:getX armor-button-size) 2.0))
              (- (/ (* 5 (*:getY armor-palette-size)) 8.0)
                 (/ (*:getY armor-button-size) 2.0)))))


;;; (compute-power-armor-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the absorb armor
;;; button


(define (compute-power-armor-button-size screen::Screen)
  (Vector2f 128 96))

;;; (make-power-armor-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed absorb armor button

(define (make-power-armor-button screen::Screen)
  (RadioButton screen "PowerArmorButton"
               (compute-power-armor-button-origin screen)
               (compute-power-armor-button-size screen)))


;;; (compute-energy-armor-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the absorb armor
;;; button


(define (compute-energy-armor-button-origin screen::Screen)
  (let ((armor-palette-size (compute-armor-palette-size screen))
        (armor-button-size (compute-absorb-armor-button-size screen)))
    (Vector2f (- (/ (*:getX armor-palette-size) 2.0)
                 (/ (*:getX armor-button-size) 2.0))
              (- (/ (* 7 (*:getY armor-palette-size)) 8.0)
                 (/ (*:getY armor-button-size) 2.0)))))


;;; (compute-energy-armor-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the absorb armor
;;; button


(define (compute-energy-armor-button-size screen::Screen)
  (Vector2f 128 96))

;;; (make-energy-armor-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed absorb armor button

(define (make-energy-armor-button screen::Screen)
  (RadioButton screen "EnergyArmorButton"
               (compute-energy-armor-button-origin screen)
               (compute-energy-armor-button-size screen)))


