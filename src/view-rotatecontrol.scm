;;;; ***********************************************************************
;;;;
;;;; Name:          view-rotatecontrol.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       a control object that rotates its Spatial
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export RotateControl)

(require "util-java.scm")

(import-as AbstractControl com.jme3.scene.control.AbstractControl)

(define-simple-class RotateControl (AbstractControl)
  (xrate type: float init: 0.0)
  ((getXRate) xrate)
  ((setXRate new-rate)(set! xrate new-rate))
  (yrate type: float init: 0.0)
  ((getYrate) yrate)
  ((setYrate new-rate)(set! yrate new-rate))
  (zrate type: float init: 0.0)
  ((getZrate) zrate)
  ((setZrate new-rate)(set! zrate new-rate))
  ((*init* xr yr zr)
   (begin
     (set! xrate xr)
     (set! yrate yr)
     (set! zrate zr)))
  ((controlUpdate tpf)
   (when (*:getSpatial (this))
     (*:rotate (*:getSpatial (this))
               (* tpf xrate)
               (* tpf yrate)
               (* tpf zrate))))
  ;; dummy update method to make Java happy
  ((controlRender renderManager viewPort) #!void))
