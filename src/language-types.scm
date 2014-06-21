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

(module-export subclass? get-superclass get-interfaces direct-superclasses all-superclasses)

(require 'list-lib)
(require "util-general.scm")
(require "util-java.scm")
(require "util-lists.scm")

(define-private-alias Class java.lang.Class)

;;; ---------------------------------------------------------------------
;;; obtaining stably-sorted lists of direct superclasses
;;; ---------------------------------------------------------------------

(define (subclass? class1::Class class2::Class)
  (if (invoke class2 'isAssignableFrom class1)
      #t
      #f))

(define (get-superclass cl::Class)
  (let ((sup (invoke cl 'getSuperclass)))
    (if (jnull? sup)
        #f
        sup)))

(define (get-interfaces cl::Class)
  (array->list (invoke cl 'getInterfaces)))

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
;;; obtaining stably-sorted lists of all superclasses
;;; ---------------------------------------------------------------------
;;; NOTE: for now we don't care about superclass linearization; we're
;;; going to use flat dispatch--i.e. either we get and exact match on
;;; the parameter list or we fail over to the default method.
;;; In future, though, it's possible we may want to add support for
;;; inherited methods, in which case we'll need stably-sorted
;;; lists of all superclasses.

#|
(define (all-superclasses cl::Class)
  (let* ((directs (direct-superclasses cl))
         (super-lists (map direct-superclasses directs))
         (supers (reverse directs)))
    (let loop ((slists super-lists))
      (if (null? slists)
          (reverse supers)
          (let ((slist (car slists)))
            (for-each (lambda (s)(set! supers (lset-adjoin eq? supers s)))
                      slist)
            (loop (cdr slists)))))))
|#

(define (all-superclasses cl::Class)
  (let* ((directs (direct-superclasses cl))
         (super-lists (map all-superclasses directs)))
    (let loop ((sups (reverse directs))
               (suplists super-lists))
      (if (null? suplists)
          (reverse sups)
          (loop (adjoin-all eq? sups (car suplists))
                (cdr suplists))))))



