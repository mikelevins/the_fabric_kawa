;;;; ***********************************************************************
;;;;
;;;; Name:          view-random-name-button.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       random names for the character creator
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 make-random-name-button
 RandomNameButton)

(require util-color)
(require util-random)
(require model-rect)
(require model-namegen)
(require model-character)
(require state)
(require state-create-character)
(require view-character-model)
(require view-name-generator)

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

;;; CLASS RandomNameButton
;;; ---------------------------------------------------------------------
;;; a SelectBox subclass used to present name options for player
;;; characters

(define-simple-class RandomNameButton (Button)
  (state::CreateCharacterState init-form: #!null)
  (palette init-form: #!null)
  ((*init* screen::Screen uid::String position::Vector2f size::Vector2f)
   (invoke-special Button (this) '*init* screen uid position size))
  ((onButtonMouseLeftDown evt::MouseButtonEvent toggled::boolean) #!void)
  ((onButtonMouseRightDown evt::MouseButtonEvent toggled::boolean) #!void)
  ((onButtonMouseLeftUp evt::MouseButtonEvent toggled::boolean)(handle-generate-random-name state))
  ((onButtonMouseRightUp evt::MouseButtonEvent toggled::boolean) #!void)
  ((onButtonFocus evt::MouseMotionEvent) #!void)
  ((onButtonLostFocus evt::MouseMotionEvent) #!void))

;;; (handle-generate-random-name)
;;; ---------------------------------------------------------------------
;;; replace the appstate's character name with a newly-generated one

(define (handle-generate-random-name state::CreateCharacterState)
  (let* ((character::FabricCharacter state:character)
         (model::Node state:character-model)
         (nameplate::Label state:character-nameplate)
         (new-name (generate-fabric-name part-count: (+ 1 (random-integer 4))))
         (faction character:faction)
         (lit-color (if (eqv? #!null faction)
                        (default-glow-color)
                        (case faction
                          ((caretakers)(bright-caretakers-color))
                          ((rogues)(bright-rogues-color))
                          ((abjurers)(bright-abjurers-color))
                          (else (default-glow-color)))))
         (dim-color (if (eqv? #!null faction)
                        (default-character-color)
                        (case faction
                          ((caretakers)(dim-caretakers-color))
                          ((rogues)(dim-rogues-color))
                          ((abjurers)(dim-abjurers-color))
                          (else (default-character-color))))))
    (set! character:name new-name)
    (*:setText nameplate (fabric-character-namestring character))
    (recolor-character-model! character model lit-color dim-color)))

;;; (make-random-name-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed random-name button

(define (make-random-name-button screen::Screen state::CreateCharacterState)
  (let* ((rect (compute-name-generator-rect screen))
         (button-width 144)
         (button-height 36)
         (button-left 16)
         (button-top 48)
         (button::RandomNameButton (RandomNameButton screen "RandomNameButton"
                                                     (Vector2f button-left button-top)
                                                     (Vector2f button-width button-height))))
    (*:setText button "Generate Name")
    (set! button:state state)
    button))

