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

(require util-java)

(import-as AbstractControl com.jme3.scene.control.AbstractControl)

(define-simple-class RotateControl (AbstractControl)
  (xrate type: float init: 0.0)
  (yrate type: float init: 0.0)
  (zrate type: float init: 0.0)
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
