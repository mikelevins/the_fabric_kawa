;;;; ***********************************************************************
;;;;
;;;; Name:          view-alert.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       alert dialogs
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 alert)

(import (class com.jme3.input.event MouseButtonEvent))
(import (class com.jme3.math Vector2f))
(import (class java.lang String))
(import (class tonegod.gui.controls.windows AlertBox))
(import (class tonegod.gui.core Screen))

(define-simple-class MessageAlert (AlertBox)
  ((*init* a-screen::Screen uid::String position::Vector2f size::Vector2f)
   (invoke-special AlertBox (this) '*init* a-screen uid position size))
  ((onButtonOkPressed evt::MouseButtonEvent toggled?::boolean)
   (let* ((win::AlertBox (this))
          (screen::Screen win:screen))
     (*:removeElement screen (this)))))

(define (alert screen::Screen msg::String)
  (let* ((screen-width (*:getWidth screen))
         (screen-height (*:getHeight screen))
         (win::MessageAlert (MessageAlert screen "Alert"
                                          (Vector2f (- (/ screen-width 2) 200)
                                                    (- (/ screen-height 2) 150))
                                          (Vector2f 400 300))))
    (*:setMsg win msg)
    (*:addElement screen win)))
