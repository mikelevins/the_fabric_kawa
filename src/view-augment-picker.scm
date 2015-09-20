;;;; ***********************************************************************
;;;;
;;;; Name:          view-pickaugment.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the augment picker 
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 make-augment-picker)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require util-error)
(require client-class)
(require client-state)
(require state-create-character)
(require model-rect)
(require view-augments-button-group)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class com.jme3.font BitmapFont))
(import (class com.jme3.math ColorRGBA  Vector2f))
(import (class tonegod.gui.controls.buttons RadioButton))
(import (class tonegod.gui.controls.windows Window))
(import (class tonegod.gui.core Screen))

;;; (compute-force-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the force faction
;;; button

(define (compute-force-button-origin screen::Screen)
  (let* ((button-width 96)
         (button-height 96)
         (rect (compute-augment-picker-rect screen))
         (palette-width (get-width rect))
         (palette-height (get-height rect))
         (x (- (* 1 (/ palette-width 8.0))
               (/ button-width 2.0)))
         (y (- (/ palette-height 2.0)
               (/ button-height 2.0))))
    (Vector2f x y)))

;;; (compute-force-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the force augment
;;; button

(define (compute-force-button-size screen::Screen)
  (Vector2f 96 96))

;;; (make-force-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed force augment button

(define (make-force-button screen::Screen)
  (RadioButton screen "ForceButton"
               (compute-force-button-origin screen)
               (compute-force-button-size screen)))


;;; (compute-optics-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the optics augment
;;; button

(define (compute-optics-button-origin screen::Screen)
  (let* ((button-width 96)
         (button-height 96)
         (rect (compute-augment-picker-rect screen))
         (palette-width (get-width rect))
         (palette-height (get-height rect))
         (x (- (* 3 (/ palette-width 8.0))
               (/ button-width 2.0)))
         (y (- (/ palette-height 2.0)
               (/ button-height 2.0))))
    (Vector2f x y)))

;;; (compute-optics-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the optics augment
;;; button

(define (compute-optics-button-size screen::Screen)
  (Vector2f 96 96))

;;; (make-optics-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed optics augment button

(define (make-optics-button screen::Screen)
  (RadioButton screen "OpticsButton"
               (compute-optics-button-origin screen)
               (compute-optics-button-size screen)))


;;; (compute-portals-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the portals augment
;;; button

(define (compute-portals-button-origin screen::Screen)
  (let* ((button-width 96)
         (button-height 96)
         (rect (compute-augment-picker-rect screen))
         (palette-width (get-width rect))
         (palette-height (get-height rect))
         (x (- (* 5 (/ palette-width 8.0))
               (/ button-width 2.0)))
         (y (- (/ palette-height 2.0)
               (/ button-height 2.0))))
    (Vector2f x y)))

;;; (compute-portals-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the portals augment
;;; button

(define (compute-portals-button-size screen::Screen)
  (Vector2f 96 96))

;;; (make-portals-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed portals augment button

(define (make-portals-button screen::Screen)
  (RadioButton screen "PortalsButton"
               (compute-portals-button-origin screen)
               (compute-portals-button-size screen)))


;;; (compute-turrets-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the turrets augment
;;; button

(define (compute-turrets-button-origin screen::Screen)
  (let* ((button-width 96)
         (button-height 96)
         (rect (compute-augment-picker-rect screen))
         (palette-width (get-width rect))
         (palette-height (get-height rect))
         (x (- (* 7 (/ palette-width 8.0))
               (/ button-width 2.0)))
         (y (- (/ palette-height 2.0)
               (/ button-height 2.0))))
    (Vector2f x y)))

;;; (compute-turrets-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the turrets augment
;;; button

(define (compute-turrets-button-size screen::Screen)
  (Vector2f 96 96))

;;; (make-turrets-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed turrets augment button

(define (make-turrets-button screen::Screen)
  (RadioButton screen "TurretsButton"
               (compute-turrets-button-origin screen)
               (compute-turrets-button-size screen)))

;;; make-character-picker
;;; ---------------------------------------------------------------------

(define (make-augment-picker state::FabricClientState screen::Screen)
  (let* ((rect (compute-augment-picker-rect screen))
         (align BitmapFont:Align)
         (valign BitmapFont:VAlign)
         (augments-group::AugmentsButtonGroup (AugmentsButtonGroup screen "AugmentsGroup"))
         (force-button (make-force-button screen))
         (optics-button (make-optics-button screen))
         (portals-button (make-portals-button screen))
         (turrets-button (make-turrets-button screen))
         (win (Window screen "AugmentPicker"
                      (Vector2f (get-left rect)(get-top rect))
                      (Vector2f (get-width rect)(get-height rect)))))
    (*:setWindowTitle win "Choose an augment:")
    (set! augments-group:app-state state)
    ;; force button
    (*:setButtonIcon force-button 96 96 "Interface/force-augment-icon96.png")
    (*:setButtonPressedInfo force-button "Interface/force-augment-icon96.png"
                            (ColorRGBA 0.0 1.0 0.0 1.0))
    (*:setText force-button "Force")
    (*:setTextAlign force-button align:Center)
    (*:setTextVAlign force-button valign:Bottom)
    (*:setFontSize force-button 20)
    (*:addButton augments-group force-button)
    (*:addChild win force-button)
    ;; optics button
    (*:setButtonIcon optics-button 96 96 "Interface/optics-augment-icon96.png")
    (*:setButtonPressedInfo optics-button "Interface/optics-augment-icon96.png"
                            (ColorRGBA 0.0 1.0 0.0 1.0))
    (*:setText optics-button "Optics")
    (*:setTextAlign optics-button align:Center)
    (*:setTextVAlign optics-button valign:Bottom)
    (*:setFontSize optics-button 20)
    (*:addButton augments-group optics-button)
    (*:addChild win optics-button)
    ;; portals button
    (*:setButtonIcon portals-button 96 96 "Interface/portals-augment-icon96.png")
    (*:setButtonPressedInfo portals-button "Interface/portals-augment-icon96.png"
                            (ColorRGBA 0.0 1.0 0.0 1.0))
    (*:setText portals-button "Portals")
    (*:setTextAlign portals-button align:Center)
    (*:setTextVAlign portals-button valign:Bottom)
    (*:setFontSize portals-button 20)
    (*:addButton augments-group portals-button)
    (*:addChild win portals-button)
    ;; turrets button
    (*:setButtonIcon turrets-button 96 96 "Interface/turrets-augment-icon96.png")
    (*:setButtonPressedInfo turrets-button "Interface/turrets-augment-icon96.png"
                            (ColorRGBA 0.0 1.0 0.0 1.0))
    (*:setText turrets-button "Turrets")
    (*:setTextAlign turrets-button align:Center)
    (*:setTextVAlign turrets-button valign:Bottom)
    (*:setFontSize turrets-button 20)
    (*:addButton augments-group turrets-button)
    (*:addChild win turrets-button)
    win))
