;;;; ***********************************************************************
;;;;
;;;; Name:          util-error.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       error and warning utilities
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************
(format #t "~%loading util-error.scm")

(module-export warn)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; conveniences for issuing errors and warnings

;;; (warn format-string args)
;;; ---------------------------------------------------------------------
;;; print a warning that includes the format-string in the output

(define-syntax warn
  (syntax-rules ()
    ((warn msg arg ...)
     (format #t (string-append "~%Warning: " msg) arg ...))))


