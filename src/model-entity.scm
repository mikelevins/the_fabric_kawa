;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          entity.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the Fabric's entity system
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export entity? ensure-entity-id empty-entity entity)

(require "model-frame.scm")
(require "model-id.scm")

(define-private-alias EMap com.github.krukow.clj_lang.PersistentHashMap)

(define (entity? thing)
  (and (instance? thing EMap)
       (contains-key? thing entity-id:)))

(define (ensure-entity-id f::EMap #!optional (id #f))
  (if (contains-key? f entity-id:)
      f
      (let ((id (or id (makeid))))
	(invoke f 'assoc entity-id: id))))

(define (empty-entity) 
  (ensure-entity-id (ensure-versioned (empty-frame)) 0))

(define (entity . inits)
  (let loop ((f :: EMap (empty-frame))
	     (kv-list inits))
    (if (null? kv-list)
	(ensure-entity-id (ensure-versioned f))
	(if (null? (cdr kv-list))
	    (error "Odd number of arguments to entity: " inits)
	    (loop (invoke f 'assoc (car kv-list)(cadr kv-list))
		  (cddr kv-list))))))

;;; (empty-entity)
;;; (entity? (empty-entity))
;;; (entity? (empty-kind))
;;; (define $p (entity name: "Paul"))
;;; (put-key $p a: 'fooo)


