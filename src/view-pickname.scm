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
(require "view-random-name-button.scm")

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
         (selector0::NameSelector (NameSelector screen "NameSelector0"  (Vector2f 16 36)(Vector2f 180 172)))
         (selector1::NameSelector (NameSelector screen "NameSelector1"  (Vector2f 204 36)(Vector2f 90 172)))
         (selector2::NameSelector (NameSelector screen "NameSelector2"  (Vector2f 302 36)(Vector2f 180 172)))
         (selector3::NameSelector (NameSelector screen "NameSelector3"  (Vector2f 490 36)(Vector2f 180 172)))
         (selector4::NameSelector (NameSelector screen "NameSelector4"  (Vector2f 678 36)(Vector2f 180 172)))
         (selector5::NameSelector (NameSelector screen "NameSelector5"  (Vector2f 866 36)(Vector2f 180 172)))
         (selector6::NameSelector (NameSelector screen "NameSelector6"  (Vector2f 1054 36)(Vector2f 180 172)))
         (selector7::NameSelector (NameSelector screen "NameSelector7"  (Vector2f 1242 36)(Vector2f 180 172)))
         (win (Window screen "NamePicker"
                      (Vector2f (get-left rect) (get-top rect))
                      (Vector2f (get-width rect) (get-height rect))))
         (random-name-button (make-random-name-button screen)))
    (*:setWindowTitle win "Choose a name from any column. You may choose from more than one column.")
    (for-each (lambda (nm)(*:addListItem selector0 nm nm))(domain0))
    (*:addChild win selector0)
    (for-each (lambda (nm)(*:addListItem selector1 nm nm))(domain1))
    (*:addChild win selector1)
    (for-each (lambda (nm)(*:addListItem selector2 nm nm))(domain2))
    (*:addChild win selector2)
    (for-each (lambda (nm)(*:addListItem selector3 nm nm))(domain3))
    (*:addChild win selector3)
    (for-each (lambda (nm)(*:addListItem selector4 nm nm))(domain4))
    (*:addChild win selector4)
    (for-each (lambda (nm)(*:addListItem selector5 nm nm))(domain5))
    (*:addChild win selector5)
    (for-each (lambda (nm)(*:addListItem selector6 nm nm))(domain6))
    (*:addChild win selector6)
    (for-each (lambda (nm)(*:addListItem selector7 nm nm))(domain7))
    (*:addChild win selector7)
    (*:addChild win random-name-button)
    win))
