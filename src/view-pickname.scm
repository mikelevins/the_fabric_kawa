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
(require "gamestates-createchar.scm")
(require "model-rect.scm")
(require "view-name-selector.scm")
(require "data-names.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as ColorRGBA com.jme3.math.ColorRGBA)
(import-as Screen tonegod.gui.core.Screen)
(import-as SelectList tonegod.gui.controls.lists.SelectList)
(import-as Vector2f com.jme3.math.Vector2f)
(import-as Window tonegod.gui.controls.windows.Window)


;;; make-name-picker
;;; ---------------------------------------------------------------------

(define (make-name-picker screen::Screen)
  (let* ((rect (compute-name-picker-rect screen))
         (selector0::NameSelector (NameSelector screen "NameSelector0"  (Vector2f 8 36)(Vector2f 180 172)))
         (selector1::NameSelector (NameSelector screen "NameSelector1"  (Vector2f 196 36)(Vector2f 90 172)))
         (win (Window screen "NamePicker"
                      (Vector2f (get-left rect) (get-top rect))
                      (Vector2f (get-width rect) (get-height rect)))))
    (*:setWindowTitle win "Choose a name:")
    (for-each (lambda (nm)(*:addListItem selector0 nm nm))(domain0))
    (*:addChild win selector0)
    (for-each (lambda (nm)(*:addListItem selector1 nm nm))(domain1))
    (*:addChild win selector1)
    win))
