;;;; ***********************************************************************
;;;;
;;;; Name:          util-random.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       random number generators
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export random-integer)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; this file provides utilities for generating pseudi-random numbers
;;; for use in Fabric code

;;; PARAMETER random-state
;;; ---------------------------------------------------------------------
;;; a parameter that stores the current random-state object used by
;;; the JVM's random-number generator. The random-state is generated
;;; from the system clock at startup and is updated each time a random
;;; number is requested.

(define random-state
  (make-parameter (java.util.Random (*:getTime (java.util.Date)))))

;;; (random-integer n)
;;; ---------------------------------------------------------------------
;;; returns the next pseudo-random integer greater than or equal to
;;; zero and less than _n_

(define (random-integer n)
  (let ((rs::java.util.Random (random-state)))
    (*:nextInt rs n)))



