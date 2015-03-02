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
  (Window screen "ArmorPalette"
          (compute-armor-palette-origin screen)
          (compute-armor-palette-size screen)))

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


