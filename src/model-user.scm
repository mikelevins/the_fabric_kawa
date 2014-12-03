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

(module-export )

(require "util-java.scm")
(require "model-frame.scm")
(require "model-id.scm")
(require "model-kind.scm")
(require "model-entity.scm")

(define user-account-kind (kind name: "User Account"))

(define (user #!key
              (username #f)
              (hashed-password #f)
              (owner-name #f)
              (billing-info #f)
              (admin-info #f)
              (characters '()))
  (if username
      (entity username: username
              hashed-password: hashed-password
              owner-name: owner-name
              billing-info: billing-info
              admin-info: admin-info
              characters: characters)
      (error "Creating a user requires a username")))

;;; (define $me (user username: "mikel"))
;;; (define $corey (user username: "corey"))

