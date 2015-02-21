;;;; ***********************************************************************
;;;;
;;;; Name:          util-functions.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       utilities for working with functions
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export complement)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as Object java.lang.Object)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; this file implements higher-order functions for convenience in
;;; Fabric code

;;; (complement fn)
;;; ---------------------------------------------------------------------
;;; given a predicate function _fn_, returns a new function that
;;; accepts the same arguments and returns the boolean inverse of what
;;; _fn_ would return if applied to the same argument. In other words,
;;; if complement is applied to _fn_ returning _fn*_, then (_fn*_ x)
;;; returns the same result as (not (_fn_ x))

(define (complement fn)
  (lambda args
    (not (apply fn args))))

