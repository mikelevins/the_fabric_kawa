;;;; ***********************************************************************
;;;;
;;;; Name:          model-user.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       Fabric user accounts
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export user user?)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; this file implements the user entity, a data structure that
;;; represents in software an authenticated user of the game

(require "util-lists.scm")
(require "model-entity.scm")

(import-as String java.lang.String)

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (user #!key username id hashed-password salt name roles)
  (let ((id (or id (makeid)))
        (salt (or salt (compute-random-salt)))
        (roles (or roles '("player"))))
    (entity 'user username: username id: id hashed-password: hashed-password
            salt: salt name: name roles: roles)))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (user? thing)
  (and (entity? thing)
       (entity-type? thing 'user)))

