;;;; ***********************************************************************
;;;; Name:          ui-workshop-inspector.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the inspector window
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export make-inspector)

(require "interface-frame.scm")

(define-private-alias AppSettings com.jme3.system.AppSettings)
(define-private-alias Vector2f com.jme3.math.Vector2f)
(define-private-alias Window tonegod.gui.controls.windows.Window)

(define (make-inspector app screen)
  (let* ((screen (get-key app gui-screen:))
         (settings::AppSettings (get-key app settings:))
         (screen-margin 8)
         (inspector-width 400)
         (inspector-height 400)
         (inspector-left (- (*:getWidth settings) inspector-width screen-margin))
         (inspector-top screen-margin)
         (win (Window screen "Inspector"
                      (Vector2f inspector-left inspector-top)
                      (Vector2f inspector-width inspector-height))))
    win))
