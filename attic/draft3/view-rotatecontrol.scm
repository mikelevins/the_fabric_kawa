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
(require "syntax-classes.scm")

(import-as AbstractControl com.jme3.scene.control.AbstractControl)

(defclass RotateControl (AbstractControl)
  (slots:
   (xrate type: float init-form: 0.0 getter: getXRate setter: setXRate)
   (yrate type: float init-form: 0.0 getter: getYRate setter: setYRate)
   (zrate type: float init-form: 0.0 getter: getZRate setter: setZRate))
  (methods:
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
   ((controlRender renderManager viewPort) #!void)))
