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
 make-fabric-user
 user-add-character!)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require 'list-lib)
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

(define (make-fabric-user #!key (username #!null)
                          (password-hash #!null) (password-salt #!null)
                          (characters '()))
  (let ((user::FabricUser (FabricUser)))
    (set! user:username username)
    (set! user:password-hash password-hash)
    (set! user:password-salt password-salt)
    (set! user:characters characters)
    user))

(define (user-add-character! user::FabricUser character::FabricCharacter)
  (let ((already (filter (lambda (ch::FabricCharacter)(fabric-name=? character:name ch:name))
                         user:characters)))
    (if (null? already)
        (set! user:characters
              (cons character user:characters))
        character)))
