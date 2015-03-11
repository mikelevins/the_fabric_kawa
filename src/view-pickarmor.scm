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

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as Screen tonegod.gui.core.Screen)
(import-as Vector2f com.jme3.math.Vector2f)
(import-as Window tonegod.gui.controls.windows.Window)


;;; make-armor-picker
;;; ---------------------------------------------------------------------

(define (make-armor-picker screen::Screen)
  (let* ((screen-width (*:getWidth screen))
         (picker-left (- screen-width 256))
         (win (Window screen "ArmorPicker"
                      (Vector2f picker-left 176)
                      (Vector2f 144 512))))
    (*:setWindowTitle win "Choose an armor:")
    win))
