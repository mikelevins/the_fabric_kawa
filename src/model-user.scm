;;;; ***********************************************************************
;;;;
;;;; Name:          model-user.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       Fabric user accounts
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export )

(require "util-lists.scm")

(import-as String java.lang.String)

;;; ---------------------------------------------------------------------
;;; reading and writing the static user database
;;; ---------------------------------------------------------------------

(define (read-users path::String)
  (with-input-from-file path
    (lambda ()
      (let loop ((result '())
                 (next (read)))
        (if (eqv? next #!eof)
            (reverse result)
            (loop (cons next result)
                  (read)))))))

;;; (define $users (read-users "/Users/mikel/Workshop/programming/the_fabric/etc/users.scm"))
;;; (define $mikel (list-ref $users 0))
;;; (getf $mikel username:)
;;; (getf $mikel password:)

