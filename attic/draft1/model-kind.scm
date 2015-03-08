;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          kind.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       types for entities
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

;;;(module-export kind? empty-kind kind)

(require "util-java.scm")
(require "model-frame.scm")
(require "model-id.scm")

(import-as PMap com.github.krukow.clj_lang.PersistentHashMap)

(define (kind? thing)
  (and (instance? thing PMap)
       (contains-key? thing kind-id:)))

(define (ensure-kind-id f::PMap #!optional (id #f))
  (if (contains-key? f kind-id:)
      f
      (let ((id (or id (makeid))))
	(invoke f 'assoc 'kind-id: id))))


(define (empty-kind) 
  (ensure-kind-id (ensure-versioned (empty-frame)) 0))

(define (kind . inits)
  (let loop ((f :: PMap (empty-frame))
	     (kv-list inits))
    (if (null? kv-list)
	(ensure-kind-id (ensure-versioned f))
	(if (null? (cdr kv-list))
	    (error "Odd number of arguments to kind: " inits)
	    (loop (invoke f 'assoc (car kv-list)(cadr kv-list))
		  (cddr kv-list))))))

;;; (empty-kind)
;;; (kind? (empty-kind))
;;; (define $p (kind name: "Planet"))
;;; (put-key $p a: 'fooo)

