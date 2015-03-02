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
 CharacterRotator
 make-character-rotator)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "syntax-classes.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as AbstractControl com.jme3.scene.control.AbstractControl)


;;; CLASS CharacterRotator (AbstractControl)
;;; ---------------------------------------------------------------------
;;; a control that slowly rotates a character that is under construction

(defclass CharacterRotator (AbstractControl)
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

;;; (make-character-creator-rotator)
;;; ---------------------------------------------------------------------
;;; returns a control that slowly rotates the character cube that is
;;; under construction
(define (make-character-rotator)
  (CharacterRotator 0.1 0.2 0.0))
