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
;;; function utilities
;;; ---------------------------------------------------------------------

(define (complement fn)
  (lambda args
    (not (apply fn args))))
