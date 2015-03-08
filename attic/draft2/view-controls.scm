;;;; ***********************************************************************
;;;;
;;;; Name:          view-controls.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       custom controls for Fabric nodes
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 RotatorControl
 make-rotator-control)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "syntax-classes.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as AbstractControl com.jme3.scene.control.AbstractControl)


;;; CLASS RotatorControl (AbstractControl)
;;; ---------------------------------------------------------------------
;;; a control that slowly rotates a character that is under construction

(defclass RotatorControl (AbstractControl)
  (slots:
   (xrate type: float init-form: 0.0 getter: getXRate setter: setXRate)
   (yrate type: float init-form: 0.0 getter: getYRate setter: setYRate)
   (zrate type: float init-form: 0.0 getter: getZRate setter: setZRate))
  (methods:
   ((*init* xr yr zr)(begin (set! xrate xr)
                            (set! yrate yr)
                            (set! zrate zr)))
   ((controlUpdate tpf)
    (when (*:getSpatial (this))
      (*:rotate (*:getSpatial (this))
                (* tpf xrate)
                (* tpf yrate)
                (* tpf zrate))))
   ;; dummy update method to make Java happy
   ((controlRender renderManager viewPort) #!void)))

;;; (make-rotator-control rx ry rz)
;;; ---------------------------------------------------------------------
;;; returns a control that slowly rotates a node
(define (make-rotator-control rx ry rz)
  (RotatorControl rx ry rz))
