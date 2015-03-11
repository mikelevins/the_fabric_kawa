;;;; ***********************************************************************
;;;;
;;;; Name:          view-acceptchar.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the name picker 
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 make-character-acceptor)

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


;;; make-character-acceptor
;;; ---------------------------------------------------------------------

(define (make-character-acceptor screen::Screen)
  (let* ((screen-width (*:getWidth screen))
         (screen-height (*:getHeight screen))
         (acceptor-left (- screen-width 416))
         (acceptor-top (- screen-height 190))
         (win (Window screen "CharacterAcceptor"
                      (Vector2f acceptor-left acceptor-top)
                      (Vector2f 384 160))))
    (*:setWindowTitle win "Accept your character:")
    win))
