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

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define users (make-parameter #f))

;;; 
;;; ---------------------------------------------------------------------
;;; 

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

;;; (read-users)
;;; (users)

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (find-username username)
  (if (not (users))
      (read-users))
  (filter (lambda (u)(equal? username (get-key u username:)))
          (users)))

;;; (find-username "mikel")
;;; (find-username "dick")
