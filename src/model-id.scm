;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          id.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       unique identifiers
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export makeid id=? string->id id->string)

(require "util-java.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(define-private-alias UUID java.util.UUID)

;;; ---------------------------------------------------------------------
;;; identifiers
;;; ---------------------------------------------------------------------

(define (makeid . args)
  (UUID:randomUUID))

(define (id=? id1 id2)
  (@ 'equals id1 id2))

(define (string->id uustr)
  (UUID:fromString uustr))

(define (id->string uuid :: UUID)
  (@ 'toString uuid))

