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
 load-player-character
 save-player-character)

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
    (sexp->user user-sexp)))

(define (save-user user::FabricUser path::String)
  (let ((user-sexp (user->sexp user)))
    (write-sexp char-sexp path)))


;;; (define $char (make-fabric-character (generate-fabric-name)))
;;; ()
;;; (define $user (make-fabric-user username: "mikel"))
;;; 
