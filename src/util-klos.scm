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
 add-method!
 all-supertypes-of
 assert-entry!
 class-object?
 class-of
 direct-supertypes-of
 generic-function
 make-generic
 make-method-table
 matching-entries
 method-table
 method-table-entries
 method-table?
 no-applicable-method
 retract-entry!
 set-method-table-entries!
 signature
 signature-more-specific?
 signature-types
 signature=?
 signature?
 subtype?
 type-object?
 )

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; klos is a simple implementation of generic functions for Kawa
;;; it does not extend the Java object model on which Kawa's Scheme types
;;; are built, and so does not require sophisticated class linearization
;;; algorithms. Class precedence for the purposes of dispatch is computed
;;; from the Java inheritance chain. The class of an object is always more
;;; specific than any interfaces the class implements, but java.lang.Object
;;; is always the least specific supertype.

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require 'list-lib)
(require "util-error.scm")
(require "util-java.scm")
(require "util-lists.scm")
(require "util-sort.scm")

;;; ---------------------------------------------------------------------
;;; imports
;;; ---------------------------------------------------------------------

(import-as ProcedureN gnu.mapping.ProcedureN)

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
      #t
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
    (sort (filter (lambda (e)(signature-more-specific? sig (car e)))
                  entries)
          (lambda (e f)(signature-more-specific? (car e)(car f))))))

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

;;; ---------------------------------------------------------------------
;;; generic functions
;;; ---------------------------------------------------------------------

;;; (no-applicable-method . args)
;;; ---------------------------------------------------------------------
;;; called when a generic function is applied to some arguments
;;; and no method applicable to the arguments can be found on the
;;; generic function. Signals an error whose message reports the
;;; arguments and their types.

(define (no-applicable-method . args)
  (error (format #f "No applicable method for arguments ~s with types ~s "
                 args (map (lambda (a)(a:getClass))
                           args))))

;;; (%apply-gf-to-args gf args default-method)
;;; ---------------------------------------------------------------------
;;; FORWARD REFERENCE

(define %apply-gf-to-args #f)

;;; CLASS generic-function
;;; ---------------------------------------------------------------------
;;; the class of generic functions. generic functions are procedures
;;; that select a method to run by examining the types of arguments
;;; passed to them.

(define-simple-class generic-function (ProcedureN)
  (default-method::ProcedureN init-form: no-applicable-method)
  (methods init-form: (make-method-table))
  ((applyN args::Object[]) (%apply-gf-to-args (this) (array->list args) default-method))
  ((addMethod method-signature::type-signature method::ProcedureN)
   (assert-entry! methods method-signature method)))

(define (make-generic)(generic-function))

(define (add-method! gf::generic-function sig::type-signature method::ProcedureN)
  (assert-entry! gf:methods sig method)
  gf)

;;; (%apply-gf-to-args gf args default-method)
;;; ---------------------------------------------------------------------
;;; a private function that executes generic function application.
;;; called when a generic function is applied to a set of arguments.

(set! %apply-gf-to-args
      (lambda (gf::generic-function args default-method)
        (let* ((mtable gf:methods)
               (sig (apply signature (map class-of args)))
               (entries (matching-entries mtable sig)))
          (if (null? entries)
              (apply default-method args)
              (apply (cdr (car entries)) args)))))

;;; (define gadd (make-generic))
;;; (add-method! gadd (signature gnu.math.IntNum gnu.math.IntNum) (lambda (x y)(+ x y)))
;;; (gadd 2 3)
;;; (add-method! gadd (signature java.lang.String java.lang.String) (lambda (x y)(string-append x y)))
;;; (gadd "foo" "bar")
;;; (gadd 2 "bar")
;;; (add-method! gadd (signature gnu.math.IntNum java.lang.String) (lambda (x y)(string-append (format #f "~a" x) y)))
;;; (gadd 2 "bar")

