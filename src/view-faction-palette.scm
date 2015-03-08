;;;; ***********************************************************************
;;;;
;;;; Name:          view-faction-palette.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the faction palette for the character creator
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 FactionButtonGroup
 compute-faction-palette-origin
 compute-faction-palette-size
 make-abjurer-button
 make-caretaker-button
 make-faction-palette
 make-rogue-button
 set-faction-palette-app-state!)

(require "util-java.scm")
(require "appstate-character-creator.scm")
(require "syntax-classes.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as BitmapFont com.jme3.font.BitmapFont)
(import-as Button tonegod.gui.controls.buttons.Button)
(import-as ColorRGBA com.jme3.math.ColorRGBA)
(import-as RadioButton tonegod.gui.controls.buttons.RadioButton)
(import-as RadioButtonGroup tonegod.gui.controls.buttons.RadioButtonGroup)
(import-as Screen tonegod.gui.core.Screen)
(import-as Vector2f com.jme3.math.Vector2f)
(import-as Window tonegod.gui.controls.windows.Window)

;;; =====================================================================
;;; FactionButtonGroup
;;; =====================================================================

;;; CLASS FactionButtonGroup
;;; ---------------------------------------------------------------------
;;; a RadioButtonGroup subclass used to present faction options to
;;; players

(defclass FactionButtonGroup (RadioButtonGroup)
  (slots:
   (app-state init-form: #!null getter: getAppState setter: setAppState))
  (methods:
   ((*init* screen::Screen uid::String)
    (invoke-special RadioButtonGroup (this) '*init* screen uid))
   ((onSelect index::int value::Button)
    (let ((button-id (*:getUID value))
          (state::CharacterCreatorAppState (get-app-state (this))))
      (cond
       ((equal? "CaretakerButton" button-id)(set-current-faction state 'caretakers))
       ((equal? "RogueButton" button-id)(set-current-faction state 'rogues))
       ((equal? "AbjurerButton" button-id)(set-current-faction state 'abjurers))
       (else (format #t "~%Unknown faction selected")))))))

;;; (get-app-state group::FactionButtonGroup)
;;; ---------------------------------------------------------------------
;;; returns _group_'s AppState object 

(define (get-app-state group::FactionButtonGroup)
  (*:getAppState group))

;;; (set-faction-palette-app-state! group::FactionButtonGroup state::CharacterCreatorAppState)
;;; ---------------------------------------------------------------------
;;; assigns _state_ to _group_'s app-state slot

(define (set-faction-palette-app-state! group::FactionButtonGroup state::CharacterCreatorAppState)
  (*:setAppState group state))

;;; =====================================================================
;;; the faction palette
;;; =====================================================================

;;; (compute-faction-palette-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the faction palette,
;;; taking into accoun the dimensions of the screen

(define (compute-faction-palette-origin screen::Screen)
  (Vector2f 10 10))


;;; (compute-faction-palette-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the faction palette,
;;; taking into accoun the dimensions of the screen


(define (compute-faction-palette-size screen::Screen)
  (Vector2f 616 200))


;;; (make-faction-palette screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed and -populated faction-palette window

(define (make-faction-palette screen::Screen)
  (let* ((align BitmapFont:Align)
         (valign BitmapFont:VAlign)
         (faction-group (FactionButtonGroup screen "FactionGroup"))
         (faction-palette (make-faction-palette screen))
         (caretaker-button (make-caretaker-button screen))
         (abjurer-button (make-abjurer-button screen))
         (rogue-button (make-rogue-button screen))
         (palette (Window screen "FactionPalette"
                          (compute-faction-palette-origin screen)
                          (compute-faction-palette-size screen))))
    ;; caretaker button
    (*:setButtonIcon caretaker-button 128 128 "Interface/caretakers-icon128.png")
    (*:setButtonPressedInfo caretaker-button "Interface/caretakers-icon-lit128.png"
                            (ColorRGBA 0.0 1.0 0.0 1.0))
    (*:setText caretaker-button "Caretakers")
    (*:setTextAlign caretaker-button align:Center)
    (*:setTextVAlign caretaker-button valign:Bottom)
    (*:setFontSize caretaker-button 20)
    (*:addButton faction-group caretaker-button)
    ;; abjurer button
    (*:setButtonIcon abjurer-button 128 128 "Interface/abjurers-icon128.png")
    (*:setButtonPressedInfo abjurer-button "Interface/abjurers-icon-lit128.png"
                            (ColorRGBA 1.0 0.0 0.0 1.0))
    (*:setText abjurer-button "Abjurers")
    (*:setTextAlign abjurer-button align:Center)
    (*:setTextVAlign abjurer-button valign:Bottom)
    (*:setFontSize abjurer-button 20)
    (*:addButton faction-group abjurer-button)
    ;; rogue button
    (*:setButtonIcon rogue-button 128 128 "Interface/rogues-icon128.png")
    (*:setButtonPressedInfo rogue-button "Interface/rogues-icon-lit128.png"
                            (ColorRGBA 0.0 0.75 1.0 1.0))
    (*:setText rogue-button "Rogues")
    (*:setTextAlign rogue-button align:Center)
    (*:setTextVAlign rogue-button valign:Bottom)
    (*:setFontSize rogue-button 20)
    (*:addButton faction-group rogue-button)
    (*:addChild palette caretaker-button)
    (*:addChild palette abjurer-button)
    (*:addChild palette rogue-button)    
    palette))


;;; (compute-caretaker-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the caretakers faction
;;; button


(define (compute-caretaker-button-origin screen::Screen)
  (let* ((button-width 128)
         (button-height 96)
         (palette-size (compute-faction-palette-size screen))
         (palette-width (*:getX palette-size))
         (palette-height (*:getY palette-size))
         (x (- (/ palette-width 6.0)
               (/ button-width 2.0)))
         (y (- (/ palette-height 2.0)
               (/ button-height 2.0))))
    (Vector2f x y)))


;;; (compute-caretaker-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the caretakers faction
;;; button


(define (compute-caretaker-button-size screen::Screen)
  (Vector2f 128 96))


;;; (make-caretaker-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed caretakers faction button

(define (make-caretaker-button screen::Screen)
  (RadioButton screen "CaretakerButton"
               (compute-caretaker-button-origin screen)
               (compute-caretaker-button-size screen)))


;;; (compute-abjurer-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the abjurers faction
;;; button

(define (compute-abjurer-button-origin screen::Screen)
  (let* ((button-width 128)
         (button-height 96)
         (palette-size (compute-faction-palette-size screen))
         (palette-width (*:getX palette-size))
         (palette-height (*:getY palette-size))
         (x (- (* 3 (/ palette-width 6.0))
               (/ button-width 2.0)))
         (y (- (/ palette-height 2.0)
               (/ button-height 2.0))))
    (Vector2f x y)))


;;; (compute-abjurer-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the abjurers faction
;;; button

(define (compute-abjurer-button-size screen::Screen)
  (Vector2f 128 96))


;;; (make-abjurer-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed abjurers faction button

(define (make-abjurer-button screen::Screen)
  (RadioButton screen "AbjurerButton"
               (compute-abjurer-button-origin screen)
               (compute-abjurer-button-size screen)))


;;; (compute-rogue-button-origin screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable origin for the rogues faction
;;; button

(define (compute-rogue-button-origin screen::Screen)
  (let* ((button-width 128)
         (button-height 96)
         (palette-size (compute-faction-palette-size screen))
         (palette-width (*:getX palette-size))
         (palette-height (*:getY palette-size))
         (x (- (* 5 (/ palette-width 6.0))
               (/ button-width 2.0)))
         (y (- (/ palette-height 2.0)
               (/ button-height 2.0))))
    (Vector2f x y)))


;;; (compute-rogue-button-size screen::Screen)
;;; ---------------------------------------------------------------------
;;; computes and returns a suitable size for the rogues faction
;;; button

(define (compute-rogue-button-size screen::Screen)
  (Vector2f 128 96))


;;; (make-rogue-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed rogues faction button

(define (make-rogue-button screen::Screen)
  (RadioButton screen "RogueButton"
               (compute-rogue-button-origin screen)
               (compute-rogue-button-size screen)))



