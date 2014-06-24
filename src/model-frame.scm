;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          frame.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       basic frame implementation
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export frame? empty-frame frame empty? contains-key? contains-val? ensure-versioned
               put-key get-key remove-key merge-keys keys vals)

(require "util-java.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(define-private-alias Iterator java.util.Iterator)
(define-private-alias Map com.github.krukow.clj_lang.PersistentHashMap)

;;; ---------------------------------------------------------------------
;;; <frame>
;;; ---------------------------------------------------------------------
;;; <frame> is an immutable associative array. it uses an efficient 
;;; functional tree representation to deliver good amortized performance.
;;; updates are nondestructive; for example, adding and removing
;;; key/value pairs creates new frames.

(define (frame? thing)
  (instance? thing Map))

(define (empty-frame) Map:EMPTY)

(define (frame . inits)
  (let loop ((f :: Map (empty-frame))
	     (kv-list inits))
    (if (null? kv-list)
	f
	(if (null? (cdr kv-list))
	    (error "Odd number of arguments to frame: " inits)
	    (loop (*:plus f (car kv-list)(cadr kv-list))
		  (cddr kv-list))))))

(define (empty? f :: Map)
  (*:isEmpty f))

(define (contains-key? f :: Map k)
  (*:containsKey f k))

(define (contains-val? f :: Map k)
  (*:containsValue f k))

(define (increment-if-version f::Map)
  (if (contains-key? f version:)
      (*:plus f  version: (+ 1 (*:valAt f version:)))
      f))

(define (ensure-versioned f::Map)
  (if (contains-key? f version:)
      f
      (*:plus f version: 0)))

;;; (define $f1 (frame))
;;; (empty? $f1)
;;; (define $f2 (frame a: 1 b: 'two c: "three"))
;;; (empty? $f2)
;;; (contains-key? $f1 b:)
;;; (contains-key? $f2 b:)

(define (put-key f :: Map k v)
  (increment-if-version (*:plus f k v)))

(define (get-key f :: Map k #!optional (default #f))
  (*:valAt f k default))

(define (remove-key f :: Map k)
  (increment-if-version (*:minus f k)))

;;; (define $f3 (put-key $f2 d: 2.3))
;;; (get-key $f3 d:)
;;; (get-key $f2 d:)
;;; (get-key $f2 d: 'nope)

(define (merge-keys f1 :: Map f2 :: Map)
  (let loop ((keys :: Iterator (*:iterator f2))
	     (f* f1))
    (if (*:hasNext keys)
	(let* ((e :: java.util.Map$Entry (*:next keys))
	       (k (*:getKey e))
	       (v (*:getValue e)))
	  (loop keys (put-key f* k v)))
	(increment-if-version f*))))

;;; (define $f4 (merge-keys $f3 (frame c: 33)))

(define (keys f :: Map)
  (let loop ((keyit :: Iterator (*:iterator f))
	     (keys '()))
    (if (*:hasNext keyit)
	(let* ((e :: java.util.Map$Entry (*:next keyit))
	       (k (*:getKey e)))
	  (loop keyit (cons k keys)))
	(reverse keys))))

;;; (keys $f3)

(define (vals f :: Map)
  (let loop ((keyit :: Iterator (*:iterator f))
	     (vals '()))
    (if (*:hasNext keyit)
	(let* ((e :: java.util.Map$Entry (*:next keyit))
	       (v (*:getValue e)))
	  (loop keyit (cons v vals)))
	(reverse vals))))

;;; (vals $f3)


