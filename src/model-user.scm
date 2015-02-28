;;;; ***********************************************************************
;;;;
;;;; Name:          model-user.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       Fabric user accounts
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 user
 user?)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; this file implements the user entity, a data structure that
;;; represents in software an authenticated user of the game

(require "util-lists.scm")
(require "model-entity.scm")

(import-as String java.lang.String)


;;; (user #!key username id hashed-password salt name roles)
;;; ---------------------------------------------------------------------
;;; returns a new user entity. _username_ is the desired username for
;;; the entity. _id_ can be an existing user id or #f; if #f then a
;;; new id is generated. _hased-password_ is a
;;; cryptographically-hashed digest of the user's password, or #f. If
;;; #f then the user account is disabled. _salt_ is a cryptographic
;;; salt used to increase the computational difficulty of brute force
;;; attacks on the passwords in the Fabric database. It should be
;;; generated using the compute-random-salt function from
;;; util-crypt.scm, should be different for each user account, and
;;; should be stored on the user entity.

(define (user #!key username id hashed-password salt name roles)
  (let ((id (or id (makeid)))
        (salt (or salt (compute-random-salt)))
        (roles (or roles '("player"))))
    (entity 'user username: username id: id hashed-password: hashed-password
            salt: salt name: name roles: roles)))


;;; (user? thing)
;;; ---------------------------------------------------------------------
;;; returns true if _thing_ is a user entity

(define (user? thing)
  (and (entity? thing)
       (entity-type? thing 'user)))

