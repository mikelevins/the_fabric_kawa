;;;; ***********************************************************************
;;;; Name:          utilities-math.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       mathematical utilities
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export degrees->radians)

(define-private-alias FastMath com.jme3.math.FastMath)

(define (degrees->radians x)
  (* FastMath:DEG_TO_RAD x))

