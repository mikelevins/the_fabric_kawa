;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          random.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       random number generators
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export random-integer)

(require "util-java.scm")

(define $random-state
  (make-parameter (java.util.Random (*:getTime (java.util.Date)))))

(define (random-integer n)
  (let ((rs::java.util.Random ($random-state)))
    (*:nextInt rs n)))



