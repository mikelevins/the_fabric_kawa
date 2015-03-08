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
(import-as ColorRGBA com.jme3.math.ColorRGBA)
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
       ((equal? "CannonWeaponButton" button-id)(set-current-weapon state 'cannon-weapons))
       ((equal? "ImpulseWeaponButton" button-id)(set-current-weapon state 'impulse-weapons))
       ((equal? "MalwareWeaponButton" button-id)(set-current-weapon state 'malware-weapons))
       ((equal? "BotsWeaponButton" button-id)(set-current-weapon state 'bots-weapons))
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
  (let ((weapons-palette (Window screen "WeaponsPalette"
                                 (compute-weapons-palette-origin screen)
                                 (compute-weapons-palette-size screen)))
        (align BitmapFont:Align)
        (valign BitmapFont:VAlign)
        (weapons-group (WeaponsButtonGroup screen "WeaponsGroup"))
        (cannon-weapon-button (make-cannon-weapon-button screen))
        (impulse-weapon-button (make-impulse-weapon-button screen))
        (malware-weapon-button (make-malware-weapon-button screen))
        (bots-weapon-button (make-bots-weapon-button screen)))
    (*:setWindowTitle weapons-palette "Choose Weapons:")
    (*:setAppState weapons-group state)
    ;; cannon weapon button
    (*:setButtonIcon cannon-weapon-button 128 128 "Interface/cannon-weapon-icon128.png")
    (*:setButtonPressedInfo cannon-weapon-button "Interface/cannon-weapon-icon128.png"
                            (ColorRGBA 1.0 1.0 0.0 1.0))
    (*:setText cannon-weapon-button "Cannon")
    (*:setTextAlign cannon-weapon-button align:Center)
    (*:setTextVAlign cannon-weapon-button valign:Bottom)
    (*:setFontSize cannon-weapon-button 20)
    (*:addButton weapons-group cannon-weapon-button)
    (*:addChild weapons-palette cannon-weapon-button)
    ;; impulse weapon button
    (*:setButtonIcon impulse-weapon-button 128 128 "Interface/impulse-weapon-icon128.png")
    (*:setButtonPressedInfo impulse-weapon-button "Interface/impulse-weapon-icon128.png"
                            (ColorRGBA 0.0 1.0 0.0 1.0))
    (*:setText impulse-weapon-button "Impulse")
    (*:setTextAlign impulse-weapon-button align:Center)
    (*:setTextVAlign impulse-weapon-button valign:Bottom)
    (*:setFontSize impulse-weapon-button 20)
    (*:addButton weapons-group impulse-weapon-button)
    (*:addChild weapons-palette impulse-weapon-button)
    ;; malware weapon button
    (*:setButtonIcon malware-weapon-button 128 128 "Interface/malware-weapon-icon128.png")
    (*:setButtonPressedInfo malware-weapon-button "Interface/malware-weapon-icon128.png"
                            (ColorRGBA 1.0 0.5 0.0 1.0))
    (*:setText malware-weapon-button "Malware")
    (*:setTextAlign malware-weapon-button align:Center)
    (*:setTextVAlign malware-weapon-button valign:Bottom)
    (*:setFontSize malware-weapon-button 20)
    (*:addButton weapons-group malware-weapon-button)
    (*:addChild weapons-palette malware-weapon-button)
    ;; bots weapon button
    (*:setButtonIcon bots-weapon-button 128 128 "Interface/bots-weapon-icon128.png")
    (*:setButtonPressedInfo bots-weapon-button "Interface/bots-weapon-icon128.png"
                            (ColorRGBA 0.0 6.0 1.0 1.0))
    (*:setText bots-weapon-button "Bots")
    (*:setTextAlign bots-weapon-button align:Center)
    (*:setTextVAlign bots-weapon-button valign:Bottom)
    (*:setFontSize bots-weapon-button 20)
    (*:addButton weapons-group bots-weapon-button)
    (*:addChild weapons-palette bots-weapon-button)
    weapons-palette))


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


