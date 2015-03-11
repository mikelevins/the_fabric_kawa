;;;; ***********************************************************************
;;;;
;;;; Name:          view-pickweapon.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the weapon picker 
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 make-weapon-picker)

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


;;; make-weapon-picker
;;; ---------------------------------------------------------------------

(define (make-weapon-picker screen::Screen)
  (let ((win (Window screen "WeaponPicker"
                     (Vector2f 16 176)(Vector2f 144 512))))
    (*:setWindowTitle win "Choose a weapon:")
    win))
