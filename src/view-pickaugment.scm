;;;; ***********************************************************************
;;;;
;;;; Name:          view-pickaugment.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the augment picker 
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 make-augment-picker)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "util-error.scm")
(require "syntax-classes.scm")
(require "client-main.scm")
(require "gamestates-createchar.scm")
(require "model-rect.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as Screen tonegod.gui.core.Screen)
(import-as Vector2f com.jme3.math.Vector2f)
(import-as Window tonegod.gui.controls.windows.Window)


;;; make-character-picker
;;; ---------------------------------------------------------------------

(define (make-augment-picker screen::Screen)
  (let* ((rect (compute-augment-picker-rect screen))
         (win (Window screen "AugmentPicker"
                      (Vector2f (get-left rect)(get-top rect))
                      (Vector2f (get-width rect)(get-height rect)))))
    (*:setWindowTitle win "Choose an augment:")
    win))
