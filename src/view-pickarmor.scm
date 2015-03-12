;;;; ***********************************************************************
;;;;
;;;; Name:          view-pickarmor.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the armor picker 
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 make-armor-picker)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "util-error.scm")
(require "syntax-classes.scm")
(require "client-main.scm")
(require "view-pickfaction.scm")
(require "gamestates-createchar.scm")
(require "model-rect.scm")


;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as Screen tonegod.gui.core.Screen)
(import-as Vector2f com.jme3.math.Vector2f)
(import-as Window tonegod.gui.controls.windows.Window)


;;; make-armor-picker
;;; ---------------------------------------------------------------------

(define (make-armor-picker screen::Screen)
  (let* ((rect (compute-armor-picker-rect screen))
         (win (Window screen "ArmorPicker"
                      (Vector2f (get-left rect)(get-top rect))
                      (Vector2f (get-width rect)(get-height rect)))))
    (*:setWindowTitle win "Choose an armor:")
    win))
