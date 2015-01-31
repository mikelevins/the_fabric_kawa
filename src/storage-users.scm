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

(require "server-config.scm")
(require "util-lists.scm")

(define users (make-parameter #f))

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

(define (find-username username)
  (filter (lambda (u)(equal? username (get-key u username:)))
          (users)))

;;; (find-username "mikel")
;;; (find-username "dick")
