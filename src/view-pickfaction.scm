;;;; ***********************************************************************
;;;;
;;;; Name:          view-pickfaction.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the faction picker 
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 make-faction-picker)

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


;;; make-character-picker
;;; ---------------------------------------------------------------------

(define (make-faction-picker screen::Screen)
  (let ((win (Window screen "FactionPicker"
                     (Vector2f 16 16)(Vector2f 512 144))))
    (*:setWindowTitle win "Choose a faction:")
    win))
