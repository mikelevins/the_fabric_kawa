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
 make-augments-palette
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
(import-as ColorRGBA com.jme3.math.ColorRGBA)
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
       ((equal? "ForceAugmentButton" button-id)(set-current-augment state 'force-augment))
       ((equal? "OpticsAugmentButton" button-id)(set-current-augment state 'optics-augment))
       ((equal? "PortalsAugmentButton" button-id)(set-current-augment state 'portals-augment))
       ((equal? "TurretsAugmentButton" button-id)(set-current-augment state 'turrets-augment))
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


;;; (make-augments-palette screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-contructed augment palette

(define (make-augments-palette screen::Screen)
  (let ((augments-palette (Window screen "AugmentsPalette"
                                  (compute-augment-palette-origin screen)
                                  (compute-augment-palette-size screen)))
        (align BitmapFont:Align)
        (valign BitmapFont:VAlign)
        (force-augment-button (make-force-augment-button screen))
        (optics-augment-button (make-optics-augment-button screen))
        (portals-augment-button (make-portals-augment-button screen))
        (turrets-augment-button (make-turrets-augment-button screen))
        (augments-group (AugmentsButtonGroup screen "AugmentsGroup")))
    (*:setWindowTitle augments-palette "Choose Augments:")
    (*:setAppState augments-group state)
    ;; force augment button
    (*:setButtonIcon force-augment-button 128 128 "Interface/force-augment-icon128.png")
    (*:setButtonPressedInfo force-augment-button "Interface/force-augment-icon128.png"
                            (ColorRGBA 1.0 1.0 0.0 1.0))
    (*:setText force-augment-button "Force Fields")
    (*:setTextAlign force-augment-button align:Center)
    (*:setTextVAlign force-augment-button valign:Bottom)
    (*:setFontSize force-augment-button 20)
    (*:addButton augments-group force-augment-button)
    (*:addChild augments-palette force-augment-button)
    ;; optics augment button
    (*:setButtonIcon optics-augment-button 128 128 "Interface/optics-augment-icon128.png")
    (*:setButtonPressedInfo optics-augment-button "Interface/optics-augment-icon128.png"
                            (ColorRGBA 0.0 1.0 0.0 1.0))
    (*:setText optics-augment-button "Optics")
    (*:setTextAlign optics-augment-button align:Center)
    (*:setTextVAlign optics-augment-button valign:Bottom)
    (*:setFontSize optics-augment-button 20)
    (*:addButton augments-group optics-augment-button)
    (*:addChild augments-palette optics-augment-button)
    ;; portals augment button
    (*:setButtonIcon portals-augment-button 128 128 "Interface/portals-augment-icon128.png")
    (*:setButtonPressedInfo portals-augment-button "Interface/portals-augment-icon128.png"
                            (ColorRGBA 1.0 0.5 0.0 1.0))
    (*:setText portals-augment-button "Portals")
    (*:setTextAlign portals-augment-button align:Center)
    (*:setTextVAlign portals-augment-button valign:Bottom)
    (*:setFontSize portals-augment-button 20)
    (*:addButton augments-group portals-augment-button)
    (*:addChild augments-palette portals-augment-button)
    ;; turrets augment button
    (*:setButtonIcon turrets-augment-button 128 128 "Interface/turrets-augment-icon128.png")
    (*:setButtonPressedInfo turrets-augment-button "Interface/turrets-augment-icon128.png"
                            (ColorRGBA 0.0 6.0 1.0 1.0))
    (*:setText turrets-augment-button "Turrets")
    (*:setTextAlign turrets-augment-button align:Center)
    (*:setTextVAlign turrets-augment-button valign:Bottom)
    (*:setFontSize turrets-augment-button 20)
    (*:addButton augments-group turrets-augment-button)
    (*:addChild augments-palette turrets-augment-button)
    augments-palette))


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


