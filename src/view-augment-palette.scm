;;;; ***********************************************************************
;;;;
;;;; Name:          view-augment-palette.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the armoraugment palette for the character creator
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 AugmentsButtonGroup
 compute-augment-palette-origin
 compute-augment-palette-size
 compute-force-augment-button-origin
 compute-force-augment-button-size
 compute-optics-augment-button-origin
 compute-optics-augment-button-size
 compute-portals-augment-button-origin
 compute-portals-augment-button-size
 compute-turrets-augment-button-origin
 compute-turrets-augment-button-size
 make-augment-palette
 make-force-augment-button
 make-optics-augment-button
 make-portals-augment-button
 make-turrets-augment-button)

(require "util-java.scm")
(require "syntax-classes.scm")
(require "view-faction-palette.scm")
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
;;; the augment palette
;;; =====================================================================

;;; CLASS AugmentsButtonGroup
;;; ---------------------------------------------------------------------
;;; a RadioButtonGroup subclass used to present augments options to
;;; players

(defclass AugmentsButtonGroup (RadioButtonGroup)
  (slots:
   (app-state init-form: #!null getter: getAppState setter: setAppState))
  (methods:
   ((*init* screen::Screen uid::String)
    (invoke-special RadioButtonGroup (this) '*init* screen uid))
   ((onSelect index::int value::Button)
    (let ((button-id (*:getUID value))
          (state::CharacterCreatorAppState app-state))
      (cond
       ((equal? "AbsorbAugmentsButton" button-id)(set-current-augment state 'force-augment))
       (else (format #t "~%Unknown augment selected")))))))

;;; (compute-augment-palette-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the augment palette, taking
;;; into account the dimensions of the screen

(define (compute-augment-palette-origin screen::Screen)
  (let ((faction-palette-origin (compute-faction-palette-origin screen))
        (faction-palette-size (compute-faction-palette-size screen))
        (screen-width (*:getWidth screen)))
    (Vector2f (- screen-width (+ 616 8))
              (*:getY faction-palette-origin))))


;;; (compute-augment-palette-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the augment palette, taking
;;; into account the dimensions of the screen

(define (compute-augment-palette-size screen::Screen)
  (let ((faction-palette-origin (compute-faction-palette-origin screen))
        (faction-palette-size (compute-faction-palette-size screen))
        (screen-width (*:getWidth screen)))
    (Vector2f 616
              (*:getY faction-palette-size))))


;;; (make-augment-palette screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-contructed augment palette

(define (make-augment-palette screen::Screen)
  (Window screen "AugmentPalette"
          (compute-augment-palette-origin screen)
          (compute-augment-palette-size screen)))


;;; (compute-force-augment-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the absorb armor
;;; button


(define (compute-force-augment-button-origin screen::Screen)
  (let ((augment-palette-size (compute-augment-palette-size screen))
        (force-augment-button-size (compute-force-augment-button-size screen)))
    (Vector2f 16
              (- (/ (*:getY augment-palette-size) 2.0)
                 (/ (*:getY force-augment-button-size) 2.0)))))


;;; (compute-force-augment-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the absorb armor
;;; button


(define (compute-force-augment-button-size screen::Screen)
  (Vector2f 128 96))

;;; (make-force-augment-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed absorb armor button

(define (make-force-augment-button screen::Screen)
  (RadioButton screen "ForceAugmentButton"
               (compute-force-augment-button-origin screen)
               (compute-force-augment-button-size screen)))


;;; (compute-optics-augment-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the absorb armor
;;; button


(define (compute-optics-augment-button-origin screen::Screen)
  (let ((augments-palette-size (compute-augment-palette-size screen))
        (optics-augment-button-size (compute-optics-augment-button-size screen)))
    (Vector2f 176
              (- (/ (*:getY augments-palette-size) 2.0)
                 (/ (*:getY optics-augment-button-size) 2.0)))))


;;; (compute-optics-augment-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the absorb armor
;;; button


(define (compute-optics-augment-button-size screen::Screen)
  (Vector2f 128 96))

;;; (make-optics-augment-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed absorb armor button

(define (make-optics-augment-button screen::Screen)
  (RadioButton screen "OpticsAugmentButton"
               (compute-optics-augment-button-origin screen)
               (compute-optics-augment-button-size screen)))


;;; (compute-portals-augment-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the absorb armor
;;; button


(define (compute-portals-augment-button-origin screen::Screen)
  (let ((augments-palette-size (compute-augment-palette-size screen))
        (portals-augment-button-size (compute-portals-augment-button-size screen)))
    (Vector2f 320
              (- (/ (*:getY augments-palette-size) 2.0)
                 (/ (*:getY portals-augment-button-size) 2.0)))))


;;; (compute-portals-augment-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the absorb armor
;;; button


(define (compute-portals-augment-button-size screen::Screen)
  (Vector2f 128 96))

;;; (make-portals-augment-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed absorb armor button

(define (make-portals-augment-button screen::Screen)
  (RadioButton screen "PortalsAugmentButton"
               (compute-portals-augment-button-origin screen)
               (compute-portals-augment-button-size screen)))


;;; (compute-turrets-augment-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the absorb armor
;;; button


(define (compute-turrets-augment-button-origin screen::Screen)
  (let ((augments-palette-size (compute-augment-palette-size screen))
        (turrets-augment-button-size (compute-turrets-augment-button-size screen)))
    (Vector2f 464
              (- (/ (*:getY augments-palette-size) 2.0)
                 (/ (*:getY turrets-augment-button-size) 2.0)))))


;;; (compute-turrets-augment-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the absorb armor
;;; button


(define (compute-turrets-augment-button-size screen::Screen)
  (Vector2f 128 96))

;;; (make-turrets-augment-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed absorb armor button

(define (make-turrets-augment-button screen::Screen)
  (RadioButton screen "TurretsAugmentButton"
               (compute-turrets-augment-button-origin screen)
               (compute-turrets-augment-button-size screen)))


