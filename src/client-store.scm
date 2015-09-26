;;;; ***********************************************************************
;;;;
;;;; Name:          client-store.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       local storage of persistent client state
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 load-user
 save-user)

(require data-sexp)
(require data-file)
(require model-character)
(require model-user)
(require client)

(import (class gnu.lists Pair))
(import (class java.io File))
(import (class java.lang String))

(define (load-user path::String)
  (let ((user-sexp (read-file path)))
    (s-expression->object user-sexp)))

(define (save-user user::FabricUser path::String)
  (let ((user-sexp (object->s-expression user)))
    (write-sexp user-sexp path)))


;;; (define $char (make-fabric-character (generate-fabric-name)))
;;; (define $user (make-fabric-user username: "mikel"))
;;; (set! $user:characters (list $char))
;;; (save-user $user "/Users/mikel/Desktop/fabric-user.sexp")
;;; (define $user2 (load-user "/Users/mikel/Desktop/fabric-user.sexp"))
;;; (equal? (object->s-expression $user)(object->s-expression $user2))
