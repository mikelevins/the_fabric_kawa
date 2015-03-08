;;;; ***********************************************************************
;;;;
;;;; Name:          storage-users.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       user account store
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export find-username read-users users)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; this file implements access to a persistent store of user
;;; entities.  it's used by the Fabric server to look up and
;;; authenticate users who attempt to connect to the game, and to
;;; authorize them for access to their in-game resources.

(require 'list-lib)
(require "server-config.scm")
(require "util-lists.scm")


;;; PARAMETER users
;;; ---------------------------------------------------------------------
;;; the table of known user records, or #f if the table has not been
;;; initialized

(define users (make-parameter #f))


;;; (read-users)
;;; ---------------------------------------------------------------------
;;; reads the statically-known user accounts from the file
;;; <fabric-root>/etc/users.scm and stores the resulting table of user
;;; entities in the parameter _users_.

(define (read-users)
  (let* ((root (fabric-root))
         (users-db-path (string-append root "etc/users.scm")))
    (with-input-from-file users-db-path
      (lambda ()
        (let loop ((records '()))
          (let ((next (read)))
            (if (eqv? #!eof next)
                (users (reverse records))
                (loop (cons next records)))))))))


;;; (find-username username)
;;; ---------------------------------------------------------------------
;;; returns the user entity whose username is _username_, or #f if no
;;; such user exists. Has the side effect of initializing the _users_
;;; parameter if it's not already initialized.

(define (find-username username)
  (if (not (users))
      (read-users))
  (filter (lambda (u)(equal? username (get-key u username:)))
          (users)))

;;; ---------------------------------------------------------------------
;;; testing code
;;; ---------------------------------------------------------------------
;;; (read-users)
;;; (users)
;;; (find-username "mikel")
;;; (find-username "dick")
