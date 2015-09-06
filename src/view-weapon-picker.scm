;;;; ***********************************************************************
;;;;
;;;; Name:          view-pickweapon.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the weapon picker 
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 make-weapon-picker)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "util-error.scm")
(require "client-class.scm")
(require "client-state.scm")
(require "model-rect.scm")
(require "view-faction-picker.scm")
(require "state-create-character.scm")
(require "view-weapons-button-group.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as BitmapFont com.jme3.font.BitmapFont)
(import-as ColorRGBA com.jme3.math.ColorRGBA)
(import-as RadioButton tonegod.gui.controls.buttons.RadioButton)
(import-as Screen tonegod.gui.core.Screen)
(import-as Vector2f com.jme3.math.Vector2f)
(import-as Window tonegod.gui.controls.windows.Window)

;;; (compute-cannon-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the cannon faction
;;; button

(define (compute-cannon-button-origin screen::Screen)
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

;;; (compute-cannon-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the cannon faction
;;; button

(define (compute-cannon-button-size screen::Screen)
  (Vector2f 96 96))

;;; (make-cannon-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed cannon faction button

(define (make-cannon-button screen::Screen)
  (RadioButton screen "CannonButton"
               (compute-cannon-button-origin screen)
               (compute-cannon-button-size screen)))


;;; (compute-impulse-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the impulse faction
;;; button

(define (compute-impulse-button-origin screen::Screen)
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

;;; (compute-impulse-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the impulse faction
;;; button

(define (compute-impulse-button-size screen::Screen)
  (Vector2f 96 96))

;;; (make-impulse-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed impulse faction button

(define (make-impulse-button screen::Screen)
  (RadioButton screen "ImpulseButton"
               (compute-impulse-button-origin screen)
               (compute-impulse-button-size screen)))


;;; (compute-malware-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the malware faction
;;; button

(define (compute-malware-button-origin screen::Screen)
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

;;; (compute-malware-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the malware faction
;;; button

(define (compute-malware-button-size screen::Screen)
  (Vector2f 96 96))

;;; (make-malware-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed malware faction button

(define (make-malware-button screen::Screen)
  (RadioButton screen "MalwareButton"
               (compute-malware-button-origin screen)
               (compute-malware-button-size screen)))


;;; (compute-bots-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the bots faction
;;; button

(define (compute-bots-button-origin screen::Screen)
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

;;; (compute-bots-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the bots faction
;;; button

(define (compute-bots-button-size screen::Screen)
  (Vector2f 96 96))

;;; (make-bots-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed bots faction button

(define (make-bots-button screen::Screen)
  (RadioButton screen "BotsButton"
               (compute-bots-button-origin screen)
               (compute-bots-button-size screen)))

;;; make-weapon-picker
;;; ---------------------------------------------------------------------

(define (make-weapon-picker state::FabricClientState screen::Screen)
  (let* ((rect (compute-weapon-picker-rect screen))
         (align BitmapFont:Align)
         (valign BitmapFont:VAlign)
         (win (Window screen "WeaponPicker"
                      (Vector2f (get-left rect)(get-top rect))
                      (Vector2f (get-width rect)(get-height rect))))
         (weapons-group (WeaponsButtonGroup screen "WeaponsGroup"))
         (cannon-button (make-cannon-button screen))
         (impulse-button (make-impulse-button screen))
         (malware-button (make-malware-button screen))
         (bots-button (make-bots-button screen)))
    (*:setWindowTitle win "Choose a weapon:")
    (set! weapons-group:app-state state)
    ;; cannon button
    (*:setButtonIcon cannon-button 96 96 "Interface/cannon-weapon-icon96.png")
    (*:setButtonPressedInfo cannon-button "Interface/cannon-weapon-icon96.png"
                            (ColorRGBA 0.0 1.0 0.0 1.0))
    (*:setText cannon-button "Cannon")
    (*:setTextAlign cannon-button align:Center)
    (*:setTextVAlign cannon-button valign:Bottom)
    (*:setFontSize cannon-button 20)
    (*:addButton weapons-group cannon-button)
    (*:addChild win cannon-button)
    ;; impulse button
    (*:setButtonIcon impulse-button 96 96 "Interface/impulse-weapon-icon96.png")
    (*:setButtonPressedInfo impulse-button "Interface/impulse-weapon-icon96.png"
                            (ColorRGBA 0.0 1.0 0.0 1.0))
    (*:setText impulse-button "Impulse")
    (*:setTextAlign impulse-button align:Center)
    (*:setTextVAlign impulse-button valign:Bottom)
    (*:setFontSize impulse-button 20)
    (*:addButton weapons-group impulse-button)
    (*:addChild win impulse-button)
    ;; malware button
    (*:setButtonIcon malware-button 96 96 "Interface/malware-weapon-icon96.png")
    (*:setButtonPressedInfo malware-button "Interface/malware-weapon-icon96.png"
                            (ColorRGBA 0.0 1.0 0.0 1.0))
    (*:setText malware-button "Malware")
    (*:setTextAlign malware-button align:Center)
    (*:setTextVAlign malware-button valign:Bottom)
    (*:setFontSize malware-button 20)
    (*:addButton weapons-group malware-button)
    (*:addChild win malware-button)
    ;; bots button
    (*:setButtonIcon bots-button 96 96 "Interface/bots-weapon-icon96.png")
    (*:setButtonPressedInfo bots-button "Interface/bots-weapon-icon96.png"
                            (ColorRGBA 0.0 1.0 0.0 1.0))
    (*:setText bots-button "Bots")
    (*:setTextAlign bots-button align:Center)
    (*:setTextVAlign bots-button valign:Bottom)
    (*:setFontSize bots-button 20)
    (*:addButton weapons-group bots-button)
    (*:addChild win bots-button)
    win))
