;;;; ***********************************************************************
;;;;
;;;; Name:          view-name-generator.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the random name generator 
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 make-name-generator)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require 'list-lib)

(require "util-java.scm")
(require "util-error.scm")
(require "client-class.scm")
(require "model-rect.scm")
(require "data-names.scm")
(require "state-create-character.scm")

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

(define (make-name-generator screen::Screen)
  (let* ((rect (compute-name-picker-rect screen))
         (win (Window screen "NameGenerator"
                      (Vector2f (get-left rect) (get-top rect))
                      (Vector2f (get-width rect) (get-height rect)))))
    (*:setWindowTitle win "Generate a random Fabric name.")
    win))

