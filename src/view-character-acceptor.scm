;;;; ***********************************************************************
;;;;
;;;; Name:          view-accept-character.scm
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

(require util-error)
(require client-class)
(require state-create-character)
(require model-rect)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class com.jme3.math Vector2f))
(import (class tonegod.gui.controls.windows Window))
(import (class tonegod.gui.core Screen))


;;; make-character-acceptor
;;; ---------------------------------------------------------------------

(define (make-character-acceptor screen::Screen)
  (let* ((name-picker-rect (compute-name-picker-rect screen))
         (screen-width (*:getWidth screen))
         (acceptor-left (+ (get-left name-picker-rect)
                           (get-width name-picker-rect)
                           8))
         (acceptor-width (- screen-width acceptor-left 8))
         (acceptor-top (get-top name-picker-rect))
         (acceptor-height (get-height name-picker-rect))
         (win (Window screen "CharacterAcceptor"
                      (Vector2f acceptor-left acceptor-top)
                      (Vector2f acceptor-width acceptor-height))))
    (*:setWindowTitle win "Accept your character:")
    win))
