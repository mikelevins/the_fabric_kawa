;;;; ***********************************************************************
;;;; Name:          application-workshop.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the builder app
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export FabricWorkshop)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "utilities-java.scm")
(require "model-frames.scm")
(require "application-common.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; FabricApp - the abstract client application class
;;; ---------------------------------------------------------------------

(define-simple-class FabricWorkshop (FabricApp)
  ;; slots
  ;; -------

  ;; accessors
  ;; ---------

  ;; implementation methods
  ;; ---------
)

;;; ---------------------------------------------------------------------
;;; accessor functions
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; application initialization
;;; ---------------------------------------------------------------------

(define (make-workshop)
  (let ((shop (FabricWorkshop)))
    ()
    shop))
