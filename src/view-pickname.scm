;;;; ***********************************************************************
;;;;
;;;; Name:          view-pickname.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the name picker 
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 make-name-picker)

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


;;; make-name-picker
;;; ---------------------------------------------------------------------

(define (make-name-picker screen::Screen)
  (let* ((screen-height (*:getHeight screen))
         (picker-top (- screen-height 190))
         (win (Window screen "NamePicker"
                      (Vector2f 16 picker-top)
                      (Vector2f 1024 160))))
    (*:setWindowTitle win "Choose a name:")
    win))
