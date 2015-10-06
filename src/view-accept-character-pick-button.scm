;;;; ***********************************************************************
;;;;
;;;; Name:          view-accept-character-pick-button.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the button that accepts a picked character
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 AcceptCharacterPickButton
 make-accept-character-pick-button)

(require client)
(require state-pick-character)


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

;;; CLASS AcceptCharacterPickButton
;;; ---------------------------------------------------------------------

(define-simple-class AcceptCharacterPickButton (Button)
  (state::PickCharacterState init-form: #!null)
  ((*init* a-state::PickCharacterState screen::Screen uid::String position::Vector2f size::Vector2f)
   (invoke-special Button (this) '*init* screen uid position size)
   (set! state a-state))
  ((onButtonMouseLeftDown evt::MouseButtonEvent toggled::boolean) #!void)
  ((onButtonMouseRightDown evt::MouseButtonEvent toggled::boolean) #!void)
  ((onButtonMouseLeftUp evt::MouseButtonEvent toggled::boolean)(handle-accept-character-pick state))
  ((onButtonMouseRightUp evt::MouseButtonEvent toggled::boolean) #!void)
  ((onButtonFocus evt::MouseMotionEvent) #!void)
  ((onButtonLostFocus evt::MouseMotionEvent) #!void))

(define (handle-accept-character-pick state::PickCharacterState)
  (let* ((character::FabricCharacter state:character)
         (client::FabricClient (the-client))
         (screen::Screen client:screen)
         (user::FabricUser client:user))
    (if (eqv? #!null user)
        (begin (format #t "No user chosen; you must log in first!")
               (activate-login-state client))
        (begin (set! client:character state:character)
               (activate-play-state client user character "Jupiter")))))

(define (make-accept-character-pick-button  screen::Screen state::PickCharacterState)
  (let* ((button-width 144)
         (button-height 36)
         (button-left 40)
         (button-top 40)
         (button::AcceptCharacterPickButton
          (AcceptCharacterPickButton state screen "AcceptCharacterPickButton"
                                     (Vector2f button-left button-top)
                                     (Vector2f button-width button-height))))
    (*:setText button "Choose Character")
    button))
