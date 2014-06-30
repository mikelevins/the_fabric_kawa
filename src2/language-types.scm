;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          language-types.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       types and linearization for gf
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export SingletonType singleton singleton? find-singleton singleton-value
               subclass? get-superclass get-interfaces direct-superclasses all-superclasses)

(require 'list-lib)
(require "utilities-java.scm")
(require "utilities-lists.scm")

(define-private-alias Class java.lang.Class)
(define-private-alias HashMap java.util.HashMap)

;;; ---------------------------------------------------------------------
;;; obtaining stably-sorted lists of direct superclasses
;;; ---------------------------------------------------------------------

(define (subclass? class1::Class class2::Class)
  (if (singleton? class2)
      (*:equals class1 class2)
      (if (singleton? class1)
          (let* ((val (singleton-value class1))
                 (vclass (val:getClass)))
            (subclass? vclass class2))
          (if (*:isAssignableFrom class2 class1)
              #t
              #f))))

(define (get-superclass cl::Class)
  (if (singleton? cl)
      (let* ((val (singleton-value cl))
             (vclass (val:getClass)))
        (get-superclass vclass))
      (let ((sup (*:getSuperclass cl)))
        (if (jnull? sup)
            #f
            sup))))

(define (get-interfaces cl::Class)
  (if (singleton? cl)
      '()
      (array->list (*:getInterfaces cl))))

(define (direct-superclasses cl::Class)
  (let* ((sup (get-superclass cl))
         (local-interfaces (get-interfaces cl))
         (local-count (length local-interfaces))
         (super-interfaces (if sup (get-interfaces sup) '()))
         (super-count (length super-interfaces))
         (supers (if sup (list sup) '())))
    (reverse (adjoin-all eq? (adjoin-all eq? supers local-interfaces)
                         super-interfaces))))


;;; ---------------------------------------------------------------------
;;; SingletonClass
;;; ---------------------------------------------------------------------
;;; used for eql specializers

(define-simple-class SingletonType (java.lang.reflect.Type)
  (value init-form: #!null)
  ((*init* val)(begin (set! value val)
                      (this)))
  ((singletonTypeValue) value))

(define singletons (make-parameter (HashMap)))

(define (singleton x)
  (let ((already (*:get (singletons) x)))
    (if (jnull? already)
        (let ((sing (SingletonType x)))
          (*:put (singletons) x sing)
          sing)
        already)))

(define (singleton-value s)
  (*:singletonTypeValue s))

(define (singleton? x)
  (instance? x SingletonType))

(define (find-singleton x)
  (let ((found (*:get (singletons) x)))
    (if (jnull? found)
        #f
        found)))

;;; ---------------------------------------------------------------------
;;; obtaining stably-sorted lists of all superclasses
;;; ---------------------------------------------------------------------
;;; NOTE: for now we don't care about superclass linearization; we're
;;; going to use flat dispatch--i.e. either we get and exact match on
;;; the parameter list or we fail over to the default method.
;;; In future, though, it's possible we may want to add support for
;;; inherited methods, in which case we'll need stably-sorted
;;; lists of all superclasses.

(define (all-superclasses cl::Class)
  (let* ((directs (direct-superclasses cl))
         (super-lists (map all-superclasses directs)))
    (let loop ((sups (reverse directs))
               (suplists super-lists))
      (if (null? suplists)
          (reverse sups)
          (loop (adjoin-all eq? sups (car suplists))
                (cdr suplists))))))



