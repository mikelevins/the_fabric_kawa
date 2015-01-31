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

(require "util-lists.scm")

(import-as String java.lang.String)

(define (user #!key username id password salt name roles)
  (let ((id (or id (makeid)))
        (salt (or salt (compute-random-salt)))
        (roles (or roles '("player"))))
    (list 'user username: username id: id password: password salt: salt name: name roles: roles)))

(define (user? thing)
  (and (pair? thing)
       (eqv? 'user (car thing))))

