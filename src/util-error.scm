;;;; ***********************************************************************
;;;;
;;;; Name:          util-error.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       error and warning utilities
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export warn)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; conveniences for issuing errors and warnings

(define-syntax warn
  (syntax-rules ()
    ((warn msg arg ...)
     (format #t (string-append "~%Warning: " msg) arg ...))))


