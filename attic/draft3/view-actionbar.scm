;;;; ***********************************************************************
;;;;
;;;; Name:          view-actionbar.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the main action bar 
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 compute-action-bar-rect
 make-action-bar)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "util-error.scm")
(require "syntax-classes.scm")
(require "client-main.scm")
(require "model-rect.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as Screen tonegod.gui.core.Screen)
(import-as Vector2f com.jme3.math.Vector2f)
(import-as Panel tonegod.gui.controls.windows.Panel)


;;; make-action-bar
;;; ---------------------------------------------------------------------

(define (compute-action-bar-rect screen::Screen)
  (let* ((screen-height (*:getHeight screen))
         (screen-width (*:getWidth screen))
         (bar-width (* 64 12))
         (bar-height (+ 64 (* 2 16)))
         (bar-left (/ (- screen-width bar-width) 2))
         (bar-top (- screen-height bar-height 16)))
    (make-rectangle bar-left
                    bar-top
                    bar-width
                    bar-height)))

(define (make-action-bar screen::Screen)
  (let* ((rect (compute-action-bar-rect screen))
         (win (Panel screen "ActionBar"
                     (Vector2f (get-left rect) (get-top rect))
                     (Vector2f (get-width rect) (get-height rect)))))
    win))
