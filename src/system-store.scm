;;;; ***********************************************************************
;;;;
;;;; Name:          system-store.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       interface to the object store
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export close-store get-root-object open-store set-root-object!)

(require "server-config.scm")
(require "util-java.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as String java.lang.String)

;;; ---------------------------------------------------------------------
;;; store parameters
;;; ---------------------------------------------------------------------

(define (open-store path::String)
  #f)

(define (close-store store::Storage)
  #f)


