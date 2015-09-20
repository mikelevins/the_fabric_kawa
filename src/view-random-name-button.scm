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

(require util-java)
(require util-color)
(require util-random)
(require model-rect)
(require model-namegen)
(require model-character)
(require client-state)
(require state-create-character)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as Button tonegod.gui.controls.buttons.Button)
(import-as Label tonegod.gui.controls.text.Label)
(import-as MouseButtonEvent com.jme3.input.event.MouseButtonEvent)
(import-as MouseMotionEvent com.jme3.input.event.MouseMotionEvent)
(import-as Screen tonegod.gui.core.Screen)
(import-as Vector2f com.jme3.math.Vector2f)
(import-as Window tonegod.gui.controls.windows.Window)

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
         (nameplate::Label state:character-nameplate)
         (new-name (generate-fabric-name part-count: (+ 1 (random-integer 4))))
         (faction character:faction)
         (lit-color (case faction
                      ((caretakers)(bright-caretakers-color))
                      ((rogues)(bright-rogues-color))
                      ((abjurers)(bright-abjurers-color))
                      (else (default-glow-color))))
         (dim-color (case faction
                      ((caretakers)(dim-caretakers-color))
                      ((rogues)(dim-rogues-color))
                      ((abjurers)(dim-abjurers-color))
                      (else (default-character-color)))))
    (set! character:name new-name)
    (*:setText nameplate (fabric-character-namestring character))
    (recolor-character-model! character lit-color dim-color)))

;;; (make-random-name-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed random-name button

(define (make-random-name-button screen::Screen state::CreateCharacterState)
  (let* ((rect (compute-name-picker-rect screen))
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

