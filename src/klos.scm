;;;; ***********************************************************************
;;;;
;;;; Name:          klos.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       generic functions and type linearization for Kawa
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 all-supertypes-of
 assert-entry!
 class-object?
 class-of
 direct-supertypes-of
 make-method-table
 matching-entries
 method-table
 method-table?
 method-table-entries
 retract-entry!
 set-method-table-entries!
 signature
 signature?
 signature-types
 signature=?
 signature-more-specific?
 subtype?
 type-object?
 )

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require 'list-lib)
(require "util-error.scm")
(require "util-java.scm")
(require "util-lists.scm")

;;; ---------------------------------------------------------------------
;;; Scheme and Java types
;;; ---------------------------------------------------------------------

(define (class-of thing)(*:getClass  thing))
(define (class-object? thing)(instance? thing java.lang.Class))
(define (type-object? thing)(instance? thing java.lang.reflect.Type))

(define (direct-supertypes-of a-class::java.lang.Class)
  (let* ((super (*:getSuperclass a-class))
         (interfaces (array->list (*:getInterfaces a-class))))
    (if (eqv? #!null super)
        '()
        (if (eqv? super java.lang.Object)
            (append interfaces (list super))
            (cons super interfaces)))))

(define (all-supertypes-of a-class::java.lang.Class)
  (let ((supers (direct-supertypes-of a-class)))
    (if (null? supers)
        '()
        (remove-duplicates (append supers (apply append (map all-supertypes-of supers)))))))

(define (subtype? thing1::java.lang.Class thing2::java.lang.Class)
  (if (eqv? thing1 thing2)
      #t
      (*:isAssignableFrom thing2 thing1)))

;;; ---------------------------------------------------------------------
;;; klos type signatures
;;; ---------------------------------------------------------------------

(define-record-type type-signature
  (%make-type-signature type-list)
  %type-signature?
  (type-list %get-types))

(define (signature . type-list)
  (%make-type-signature type-list))

(define (signature? thing)
  (%type-signature? thing))

(define (signature-types thing)
  (%get-types thing))

(define (signature=? s1 s2)
  (equal? (signature-types s1)
          (signature-types s2)))

(define (signature-more-specific? s1 s2)
  (if (eqv? s1 s2)
      #f
      (every? subtype?
              (signature-types s1)
              (signature-types s2))))

;;; ---------------------------------------------------------------------
;;; method tables
;;; ---------------------------------------------------------------------
;;; storage for methods associated with type signatures

(define-record-type method-table
  (%make-method-table entries)
  method-table?
  (entries method-table-entries set-method-table-entries!))

(define (make-method-table)
  (%make-method-table '()))

(define (matching-entries mtable sig)
  (let* ((entries (method-table-entries mtable)))
    (filter (lambda (e)(or (signature=? sig (car e))
                           (signature-more-specific? sig (car e))))
            entries)))

(define (%find-method-entry-for mtable sig)
  (let* ((entries (method-table-entries mtable)))
    (let loop ((candidates entries))
      (if (null? candidates)
          #f
          (if (signature=? sig (car (car candidates)))
              (car candidates)
              (loop (cdr candidates)))))))

(define (assert-entry! mtable sig m)
  (let ((entry (%find-method-entry-for mtable sig)))
    (if entry
        (begin (set-cdr! entry m)
               mtable)
        (let ((e (cons sig m)))
          (set-method-table-entries! mtable
                                     (cons e (method-table-entries mtable)))
          mtable))))


(define (retract-entry! mtable sig)
  (let ((entry (%find-method-entry-for mtable sig)))
    (if entry
        (begin (set-method-table-entries! mtable
                                          (remove (lambda (e)(signature=? sig (car e)))
                                                  (method-table-entries mtable)))
               mtable)
        mtable)))
