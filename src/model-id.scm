;;;; ***********************************************************************
;;;;
;;;; Name:          id.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       unique identifiers
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 id->string
 id=?
 makeid
 string->id)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; The functions in this file compute UUID objects for use
;;; as unique identifiers in the Fabric system. We use these ids
;;; for game objects like nodes and characters, and also to
;;; identify messages passed between client and server.

(require "util-java.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as UUID java.util.UUID)

;;; ---------------------------------------------------------------------
;;; identifiers
;;; ---------------------------------------------------------------------

(define (id=? id1 id2)
  (*:equals id1 id2))

(define (id->string uuid :: UUID)
  (*:toString uuid))

(define (makeid . args)
  (UUID:randomUUID))

(define (string->id uustr)
  (UUID:fromString uustr))

