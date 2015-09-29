;;;; ***********************************************************************
;;;;
;;;; Name:          view-save-character-button.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       save a constructed character
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 make-save-character-button
 SaveCharacterButton)

(require util-error)
(require util-color)
(require util-random)
(require model-rect)
(require model-namegen)
(require model-character)
(require client)
(require state)
(require state-create-character)
(require view-alert)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class com.jme3.input.event MouseButtonEvent MouseMotionEvent))
(import (class com.jme3.math Vector2f))
(import (class com.jme3.scene Node))
(import (class tonegod.gui.controls.buttons Button))
(import (class tonegod.gui.controls.text Label))
(import (class tonegod.gui.controls.windows Window))
(import (class tonegod.gui.core Screen))

;;; CLASS SaveCharacterButton
;;; ---------------------------------------------------------------------

(define-simple-class SaveCharacterButton (Button)
  (state::CreateCharacterState init-form: #!null)
  ((*init* screen::Screen uid::String position::Vector2f size::Vector2f)
   (invoke-special Button (this) '*init* screen uid position size))
  ((onButtonMouseLeftDown evt::MouseButtonEvent toggled::boolean) #!void)
  ((onButtonMouseRightDown evt::MouseButtonEvent toggled::boolean) #!void)
  ((onButtonMouseLeftUp evt::MouseButtonEvent toggled::boolean)(handle-save-current-character state))
  ((onButtonMouseRightUp evt::MouseButtonEvent toggled::boolean) #!void)
  ((onButtonFocus evt::MouseMotionEvent) #!void)
  ((onButtonLostFocus evt::MouseMotionEvent) #!void))

;;; (handle-save-current-character state)
;;; ---------------------------------------------------------------------

(define (handle-save-current-character state::CreateCharacterState)
  (let* ((character::FabricCharacter state:character)
         (client::FabricClient state:client)
         (screen::Screen client:screen)
         (user::FabricUser client:user))
    (if (eqv? #!null user)
        (alert screen (format #f "No user chosen; you must log in first!"))
        (if (eqv? #!null character)
            (alert screen (format #f "No character! Try logging in again."))
            (if (fabric-name=? character:name (blank-fabric-name))
                (alert screen (format #f "Your character has no name! Choose a name before saving."))
                (let ((already (filter (lambda (ch::FabricCharacter)(fabric-name=? character:name ch:name))
                                       user:characters)))
                  (if (null? already)
                      (begin (user-add-character! user character)
                             (save-user user))
                      (begin
                        (alert screen (format #f "You already have a character named ~A"
                                              (fabric-name->string character:name)))))))))))

;;; (make-save-character-button screen::Screen)
;;; ---------------------------------------------------------------------

(define (make-save-character-button screen::Screen state::CreateCharacterState)
  (let* ((button-width 144)
         (button-height 36)
         (button-left 40)
         (button-top 40)
         (button::SaveCharacterButton (SaveCharacterButton screen "SaveCharacterButton"
                                                        (Vector2f button-left button-top)
                                                        (Vector2f button-width button-height))))
    (*:setText button "Save Character")
    (set! button:state state)
    button))

