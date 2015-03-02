;;;; ***********************************************************************
;;;;
;;;; Name:          view-weapons-palette.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the weapons palette for the character creator
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 compute-bots-weapon-button-origin
 compute-bots-weapon-button-size
 compute-cannon-weapon-button-origin
 compute-cannon-weapon-button-size
 compute-impulse-weapon-button-origin
 compute-impulse-weapon-button-size
 compute-malware-weapon-button-origin
 compute-malware-weapon-button-size
 compute-weapons-palette-origin
 compute-weapons-palette-size
 make-bots-weapon-button
 make-cannon-weapon-button
 make-impulse-weapon-button
 make-malware-weapon-button
 make-weapons-palette
 WeaponsButtonGroup)


(require "util-java.scm")
(require "syntax-classes.scm")
(require "view-faction-palette.scm")
(require "view-name-palette.scm")
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
;;; the weapons palette
;;; =====================================================================

;;; CLASS WeaponsButtonGroup
;;; ---------------------------------------------------------------------
;;; a RadioButtonGroup subclass used to present weapons options to
;;; players

(defclass WeaponsButtonGroup (RadioButtonGroup)
  (slots:
   (app-state init-form: #!null getter: getAppState setter: setAppState))
  (methods:
   ((*init* screen::Screen uid::String)
    (invoke-special RadioButtonGroup (this) '*init* screen uid))
   ((onSelect index::int value::Button)
    (let ((button-id (*:getUID value))
          (state::CharacterCreatorAppState app-state))
      (cond
       ((equal? "CannonWeaponsButton" button-id)(set-current-weapon state 'cannon-weapons))
       (else (format #t "~%Unknown weapon selected")))))))

;;; (compute-weapons-palette-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the weapons palette, taking
;;; into account the dimensions of the screen

(define (compute-weapons-palette-origin screen::Screen)
  (let ((faction-palette-origin (compute-faction-palette-origin screen))
        (faction-palette-size (compute-faction-palette-size screen)))
    (Vector2f 10
              (+ (*:getY faction-palette-origin)
                 (*:getY faction-palette-size)
                 8))))


;;; (compute-weapons-palette-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the weapons palette, taking
;;; into account the dimensions of the screen

(define (compute-weapons-palette-size screen::Screen)
  (let* ((weapons-palette-origin (compute-weapons-palette-origin screen))
         (name-palette-origin (compute-name-palette-origin screen))
         (height (- (*:getY name-palette-origin)
                    (*:getY weapons-palette-origin)
                    8))
         (screen-width (*:getWidth screen))
         (width (- (/ screen-width 8.0) 10)))
    (Vector2f width height)))


;;; (make-weapons-palette screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-contructed weapons palette

(define (make-weapons-palette screen::Screen)
  (Window screen "WeaponsPalette"
          (compute-weapons-palette-origin screen)
          (compute-weapons-palette-size screen)))


;;; (compute-cannon-weapon-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the cannon weapon
;;; button


(define (compute-cannon-weapon-button-origin screen::Screen)
  (let ((weapons-palette-size (compute-weapons-palette-size screen))
        (weapon-button-size (compute-cannon-weapon-button-size screen)))
    (Vector2f (- (/ (*:getX weapons-palette-size) 2.0)
                 (/ (*:getX weapon-button-size) 2.0))
              (- (/ (*:getY weapons-palette-size) 8.0)
                 (/ (*:getY weapon-button-size) 2.0)))))


;;; (compute-cannon-weapon-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the cannon weapon
;;; button


(define (compute-cannon-weapon-button-size screen::Screen)
  (Vector2f 128 96))

;;; (make-cannon-weapon-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed cannon weapon button

(define (make-cannon-weapon-button screen::Screen)
  (RadioButton screen "CannonWeaponButton"
               (compute-cannon-weapon-button-origin screen)
               (compute-cannon-weapon-button-size screen)))


;;; (compute-impulse-weapon-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the impulse weapon
;;; button


(define (compute-impulse-weapon-button-origin screen::Screen)
  (let ((weapons-palette-size (compute-weapons-palette-size screen))
        (weapon-button-size (compute-cannon-weapon-button-size screen)))
    (Vector2f (- (/ (*:getX weapons-palette-size) 2.0)
                 (/ (*:getX weapon-button-size) 2.0))
              (- (/ (* 3 (*:getY weapons-palette-size)) 8.0)
                 (/ (*:getY weapon-button-size) 2.0)))))


;;; (compute-impulse-weapon-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the impulse weapon
;;; button


(define (compute-impulse-weapon-button-size screen::Screen)
  (Vector2f 128 96))

;;; (make-impulse-weapon-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed impulse weapon button

(define (make-impulse-weapon-button screen::Screen)
  (RadioButton screen "ImpulseWeaponButton"
               (compute-impulse-weapon-button-origin screen)
               (compute-impulse-weapon-button-size screen)))


;;; (compute-malware-weapon-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the malware weapon
;;; button


(define (compute-malware-weapon-button-origin screen::Screen)
  (let ((weapons-palette-size (compute-weapons-palette-size screen))
        (weapon-button-size (compute-cannon-weapon-button-size screen)))
    (Vector2f (- (/ (*:getX weapons-palette-size) 2.0)
                 (/ (*:getX weapon-button-size) 2.0))
              (- (/ (* 5 (*:getY weapons-palette-size)) 8.0)
                 (/ (*:getY weapon-button-size) 2.0)))))


;;; (compute-malware-weapon-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the malware weapon
;;; button


(define (compute-malware-weapon-button-size screen::Screen)
  (Vector2f 128 96))

;;; (make-malware-weapon-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed malware weapon button

(define (make-malware-weapon-button screen::Screen)
  (RadioButton screen "MalwareWeaponButton"
               (compute-malware-weapon-button-origin screen)
               (compute-malware-weapon-button-size screen)))


;;; (compute-bots-weapon-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the bots weapon
;;; button


(define (compute-bots-weapon-button-origin screen::Screen)
  (let ((weapons-palette-size (compute-weapons-palette-size screen))
        (weapon-button-size (compute-cannon-weapon-button-size screen)))
    (Vector2f (- (/ (*:getX weapons-palette-size) 2.0)
                 (/ (*:getX weapon-button-size) 2.0))
              (- (/ (* 7 (*:getY weapons-palette-size)) 8.0)
                 (/ (*:getY weapon-button-size) 2.0)))))


;;; (compute-bots-weapon-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the bots weapon
;;; button


(define (compute-bots-weapon-button-size screen::Screen)
  (Vector2f 128 96))

;;; (make-bots-weapon-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed bots weapon button

(define (make-bots-weapon-button screen::Screen)
  (RadioButton screen "BotsWeaponButton"
               (compute-bots-weapon-button-origin screen)
               (compute-bots-weapon-button-size screen)))


