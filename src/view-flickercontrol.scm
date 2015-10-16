;;;; ***********************************************************************
;;;;
;;;; Name:          view-flickercontrol.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       a control object that makes its Spatial flicker
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export FlickerControl)

(require util-random)

(import (class com.jme3.scene.control AbstractControl))

(define-simple-class FlickerControl (AbstractControl)
  (dim-color type: ColorRGBA init-form: #!null)
  (bright-color type: ColorRGBA init-form: #!null)
  (activity type: int init-form: 96)
  ((*init* dim bright act)
   (begin
     (set! dim-color dim)
     (set! bright-color bright)
     (set! activity act)))
  ((controlUpdate tpf)
   (let ((geom::Geometry (*:getSpatial (this))))
     (unless (eqv? #!null geom)
       (let ((mat::Material (*:getMaterial geom))
             (die (random-integer activity)))
         (if (< die 4)
             (*:setColor mat "GlowColor" bright-color)
             (*:setColor mat "GlowColor" dim-color))))))
  ;; dummy update method to make Java happy
  ((controlRender renderManager viewPort) #!void))
