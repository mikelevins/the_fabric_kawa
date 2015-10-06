;;;; ***********************************************************************
;;;;
;;;; Name:          model-user.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       modeling users and accounts
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 FabricUser
 default-user
 make-fabric-user
 user-add-character!)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require 'list-lib)
(require util-error)
(require util-lists)
(require model-character)
(require model-namegen)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class com.jme3.network.serializing Serializable))
(import (class java.lang String))

;;; ---------------------------------------------------------------------
;;; FabricCharacter
;;; ---------------------------------------------------------------------

(define-simple-class FabricUser ()(@Serializable)
  (username::String init: #!null)
  (password-hash::String init: #!null)
  (password-salt::byte[] init: #!null)
  (characters init: '()))

(define (make-fabric-user #!key (username #!null) (password-hash #!null) (password-salt #!null))
  (let ((user::FabricUser (FabricUser)))
    (set! user:username username)
    (set! user:password-hash password-hash)
    (set! user:password-salt password-salt)
    user))

(define (user-add-character! user::FabricUser character::FabricCharacter)
  (let ((already (filter (lambda (ch::FabricCharacter)(fabric-name=? character:name ch:name))
                         user:characters)))
    (if (null? already)
        (set! user:characters
              (cons character user:characters))
        character)))

;;; a default user account used for testing purposes
(define (default-user)
  (let ((user::FabricUser (make-fabric-user username: "fabric"
                                            password-hash: "rAjb1fZLVAsp6TajTiC5PMeUs2M="
                                            password-salt: (byte[] -62 -53 39 -116 -1 -14 -46 21))))
    (set! user:characters (list (default-character)(default-character)(default-character)))
    user))
