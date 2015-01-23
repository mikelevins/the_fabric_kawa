;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          model-user.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       types for user accounts
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export user)

(require "util-java.scm")
(require "model-frame.scm")
(require "model-id.scm")

(define user-account-kind (kind name: "User Account"))

(define (user #!key
              (userid #f)
              (username #f)
              (hashed-password #f)
              (owner-name #f)
              (billing-info #f)
              (admin-info #f)
              (characters '()))
  (if username
      (frame userid: (or userid (id->string (makeid)))
             username: username
             hashed-password: hashed-password
             owner-name: owner-name
             billing-info: billing-info
             admin-info: admin-info
             characters: characters)
      (error "Creating a user requires a username")))

;;; (define $me (user username: "mikel"))
;;; (define $corey (user username: "corey"))

