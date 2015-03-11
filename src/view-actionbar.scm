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
 make-action-bar)

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
(import-as Panel tonegod.gui.controls.windows.Panel)


;;; make-action-bar
;;; ---------------------------------------------------------------------

(define (make-action-bar screen::Screen)
  (let* ((screen-height (*:getHeight screen))
         (screen-width (*:getWidth screen))
         (bar-left 384)
         (bar-top (- screen-height 112))
         (bar-width (- screen-width (* 2 384)))
         (win (Panel screen "ActionBar"
                     (Vector2f bar-left bar-top)
                     (Vector2f bar-width 96))))
    win))
