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

(module-export Anything SingletonType singleton singleton? find-singleton singleton-value
               subclass? get-superclass get-interfaces direct-superclasses all-superclasses)

(require 'list-lib)
(require "utilities-java.scm")
(require "utilities-lists.scm")

(define-private-alias Class java.lang.Class)
(define-private-alias HashMap java.util.HashMap)

;;; ---------------------------------------------------------------------
;;; type utilities
;;; ---------------------------------------------------------------------

(define (class-of thing)
  (*:getClass thing))

;;; ---------------------------------------------------------------------
;;; Anything
;;; ---------------------------------------------------------------------
;;; used for default specializers

(define-simple-class Anything (java.lang.reflect.Type))

;;; ---------------------------------------------------------------------
;;; obtaining stably-sorted lists of superclasses
;;; ---------------------------------------------------------------------

(define (subclass? class1::Class class2::Class)
  (if (eq? class2 Anything)
      #t
      (if (singleton? class2)
          (*:equals class1 class2)
          (if (singleton? class1)
              (let* ((val (singleton-value class1))
                     (vclass (val:getClass)))
                (subclass? vclass class2))
              (if (*:isAssignableFrom class2 class1)
                  #t
                  #f)))))

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

(define (direct-superclasses a-class::Class)
  (let* ((super (get-superclass a-class))
         (direct-interfaces (get-interfaces a-class))
         (super-interfaces (if (eq? #!null super)
                               '()
                               (get-interfaces super))))
    (let loop1 ((supers (list a-class))
                (directs direct-interfaces))
      (if (null? directs)
          (let loop2 ((supers (if (eq? #!null super)
                                  supers
                                  (cons super supers)))
                      (indirects super-interfaces))
            (if (null? indirects)
                (reverse supers)
                (let ((ind (car indirects))
                      (more (cdr indirects)))
                  (loop2 (if (member ind supers)
                             supers
                             (cons ind supers))
                         (cdr indirects)))))
          (loop1 (cons (car directs)
                       supers)
                 (cdr directs))))))


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
  (let* ((smap::HashMap (singletons))
         (already (*:get smap x)))
    (if (jnull? already)
        (let ((sing::SingletonType (SingletonType x)))
          (*:put smap x sing)
          sing)
        already)))

(define (singleton-value s::SingletonType)
  (*:singletonTypeValue s))

(define (singleton? x)
  (instance? x SingletonType))

(define (find-singleton x)
  (let* ((smap::HashMap (singletons))
         (found (*:get smap x)))
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

(define (all-superclasses a-class::Class)
  (let ((direct-supers (cdr (direct-superclasses a-class))))
    (if (null? direct-supers)
        (list a-class)
        (let ((cpls (map all-superclasses direct-supers)))
          (cons a-class (apply append cpls))))))
