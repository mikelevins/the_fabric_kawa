;;;; ***********************************************************************
;;;;
;;;; Name:          view-error-okay-button.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       an Okay button for warning dialogs
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 ErrorOkayButton)

(require client)

(import (class com.jme3.input.event MouseButtonEvent MouseMotionEvent))
(import (class com.jme3.math Vector2f))
(import (class java.lang String))
(import (class tonegod.gui.controls.buttons Button))
(import (class tonegod.gui.core Screen))
(import (class tonegod.gui.controls.windows Panel))

(define-simple-class ErrorOkayButton (Button)
  (client::FabricClient init-form: #!null)
  (screen::Screen init-form: #!null)
  (panel::Panel init-form: #!null)
  (okay-proc init-form: #!null)
  ((*init* a-client::FabricClient a-screen::Screen
           a-panel::Panel text::String a-proc position::Vector2f size::Vector2f)
   (begin (invoke-special Button (this) '*init* a-screen "ErrorOkayButton" position size)
          (set! client a-client)
          (set! screen a-screen)
          (set! panel a-panel)
          (set! okay-proc a-proc)
          (*:setText (this) text)))
  ((onButtonMouseLeftDown evt::MouseButtonEvent toggled::boolean) #!void)
  ((onButtonMouseRightDown evt::MouseButtonEvent toggled::boolean) #!void)
  ((onButtonMouseLeftUp evt::MouseButtonEvent toggled::boolean)
   (*:enqueue client (runnable (lambda ()
                                 (begin (*:removeElement screen panel)
                                        (okay-proc client screen panel))))))
  ((onButtonMouseRightUp evt::MouseButtonEvent toggled::boolean) #!void)
  ((onButtonFocus evt::MouseMotionEvent) #!void)
  ((onButtonLostFocus evt::MouseMotionEvent) #!void))
