;;;; ***********************************************************************
;;;;
;;;; Name:          view-pickcharacter.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the character picker 
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 make-character-picker)

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

(define (make-character-picker screen::Screen)
  (let ((win (Window screen "CharacterPicker"
                     (Vector2f 32 32)(Vector2f 256 800))))
    (*:setWindowTitle win "Choose a character:")
    win))
