;;;; ***********************************************************************
;;;;
;;;; Name:          data-config.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       storing user account data
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 get-user
 get-users-path
 load-user
 save-user)

(require parameters)
(require model-user)
(require data-config)
(require data-file)
(require data-sexp)
(require client)

(import (srfi :69 basic-hash-tables))
(import (class java.lang String))

(define users (make-parameter (make-hash-table equal?)))

(define (save-user user::FabricUser)
  (let ((username user:username))
    (hash-table-set! (users) username user)
    (let* ((users-path (get-users-path))
           (save-path (string-append users-path "/" username ".sexp"))
           (user-sexp (object->s-expression user)))
      (write-sexp user-sexp save-path)
      username)))

(define (load-user username::String)
  (let* ((users-path (get-users-path))
         (load-path (string-append users-path "/" username ".sexp")))
    (if (file-exists? load-path)
        (let* ((client::FabricClient (the-client))
               (user-sexp (read-file load-path))
               (user (s-expression->object user-sexp)))
          (hash-table-set! (users) username user)
          (set! client:current-user user)
          user)
        #!null)))

;;; TODO: the default user "fabric" is used for demos
;;; it should not exist in a released version of the Fabric
(define (get-user username::String)
  (if (string=? "fabric" username)
      (get-default-user)
      (let ((found (hash-table-ref/default (users) username #!null)))
        (if (eqv? #!null found)
            (load-user username)
            found))))

(define (get-users-path)
  (let ((conf-path (get-configuration-path)))
    (string-append conf-path "/users")))

;;; (define $pwdigest (text->digest "foobar" (compute-random-salt)))
;;; (define $pw (car $pwdigest))
;;; (define $pwsalt (cdr $pwdigest))
;;; (define $fabric (make-fabric-user username: "fabric" password-hash: $pw password-salt: $pwsalt))
;;; (save-user $fabric)
;;; (define $fabric2 (get-user "fabric"))
