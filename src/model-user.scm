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

(require util-error)
(require util-lists)
(require model-character)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class java.lang String))

;;; ---------------------------------------------------------------------
;;; FabricCharacter
;;; ---------------------------------------------------------------------

(define-simple-class FabricUser ()
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
  (unless (member character user:characters)
    (set! user:characters
          (cons character
                user:characters))))
