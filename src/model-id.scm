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

;;; (id=? id1 id2)
;;; ---------------------------------------------------------------------
;;; returns true if id1 and id2 are the same id, even if they are not
;;; represented by the same object

(define (id=? id1 id2)
  (*:equals id1 id2))

;;; (id->string uuid :: UUID)
;;; ---------------------------------------------------------------------
;;; returns a string in standard UUID format that is equivalent to _uuid_

(define (id->string uuid :: UUID)
  (*:toString uuid))

;;; (makeid)
;;; ---------------------------------------------------------------------
;;; returns a new UUID object

(define (makeid)
  (UUID:randomUUID))


;;; (string->id uustr)
;;; ---------------------------------------------------------------------
;;; converts a string in standard UUID format to a UUID object

(define (string->id uustr)
  (UUID:fromString uustr))

