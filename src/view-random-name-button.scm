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

(require "util-java.scm")
(require "appstate-character-creator.scm")
(require "syntax-classes.scm")
(require "view-name-palette.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as Button tonegod.gui.controls.buttons.Button)
(import-as MouseButtonEvent com.jme3.input.event.MouseButtonEvent)
(import-as MouseMotionEvent com.jme3.input.event.MouseMotionEvent)
(import-as Screen tonegod.gui.core.Screen)
(import-as Vector2f com.jme3.math.Vector2f)
(import-as Window tonegod.gui.controls.windows.Window)

;;; CLASS RandomNameButton
;;; ---------------------------------------------------------------------
;;; a SelectBox subclass used to present name options for player
;;; characters

(defclass RandomNameButton (Button)
  (slots:
   (palette init-form: #!null :getter getPalette :setter setPalette))
  (methods:
   ((*init* screen::Screen uid::String window::Window position::Vector2f size::Vector2f)
    (invoke-special Button (this) '*init* screen uid position size)
    (set! palette window))
   ((onButtonMouseLeftDown evt::MouseButtonEvent toggled::boolean) #!void)
   ((onButtonMouseRightDown evt::MouseButtonEvent toggled::boolean) #!void)
   ((onButtonMouseLeftUp evt::MouseButtonEvent toggled::boolean)
    (pick-a-random-name palette))
   ((onButtonMouseRightUp evt::MouseButtonEvent toggled::boolean) #!void)
   ((onButtonFocus evt::MouseMotionEvent) #!void)
   ((onButtonLostFocus evt::MouseMotionEvent) #!void)))


;;; (make-random-name-button screen::Screen)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed random-name button

(define (make-random-name-button palette::Window screen::Screen)
  (let* ((palette-size::Vector2f (compute-name-palette-size screen))
         (button-width 128)
         (button-height 32)
         (button-x 64)
         (button-y (- (*:getY palette-size) button-height 24))
         (button::RandomNameButton (RandomNameButton screen "RandomNameButton"
                                                     palette
                                                     (Vector2f button-x button-y)
                                                     (Vector2f button-width button-height))))
    (*:setText button "Random Name")
    button))

