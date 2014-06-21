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

(define random-integer
  (lambda (n)
    (let ((rs::java.util.Random (java.util.Random (invoke (java.util.Date) 'getTime))))
      (@ 'nextInt rs n))))



