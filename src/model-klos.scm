;;;; ***********************************************************************
;;;;
;;;; Name:          model-klos.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       generic functions for Kawa
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 add-method!
 all-superclasses
 defgeneric
 defmethod
 direct-superclasses
 generic-function
 get-class
 get-interfaces
 no-applicable-method
 subclass?
 type-object?)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; This file implements a simplified form of CLOS-style generic
;;; functions that work with Kawa and Java types. Generic functions
;;; are useful for providing uniform extensible APIs that must work on
;;; open sets of types--for example, for providing common sets of
;;; functions to construct and deconstruct structures that are
;;; represented by various different types, or for serializing
;;; a variety of types of data.

(require 'list-lib)
(require "util-sort.scm")
(require "util-functions.scm")
(require "util-java.scm")
(require "util-lists.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as Class java.lang.Class)
(import-as Object java.lang.Object)
(import-as ProcedureN gnu.mapping.ProcedureN)
(import-as Type java.lang.reflect.Type)

;;; =====================================================================
;;; types, classes, and superclasses
;;; =====================================================================


;;; (type-object? thing)
;;; ---------------------------------------------------------------------
;;; returns true if _thing_ is a type

(define (type-object? thing)
  (instance? thing Type))

;;; (subclass? tp1::Class tp2::Class)
;;; ---------------------------------------------------------------------
;;; returns true is _tp1_ is a subclass of _tp2_. Any type is
;;; always a subclass of itself.

(define (subclass? tp1::Class tp2::Class)
  (*:isAssignableFrom tp2 tp1))

;;; (get-class thing::Object)
;;; ---------------------------------------------------------------------
;;; returns the Java class of _thing_

(define (get-class thing::Object)
  (*:getClass thing))

;;; (get-interfaces a-class::Class)
;;; ---------------------------------------------------------------------
;;; returns the interfaces implemented by _thing_

(define (get-interfaces a-class::Class)
  (gnu.lists.LList:makeList (*:getInterfaces a-class) 0))

;;; (direct-superclasses a-class::Class)
;;; ---------------------------------------------------------------------
;;; returns a list containing the superclass of _a-class_ and all
;;; interfaces that it implements

(define (direct-superclasses a-class::Class)
  (let* ((super (*:getSuperclass a-class))
         (supers (if (eqv? #!null super)
                     '()
                     (list super)))
         (direct-interfaces (get-interfaces a-class)))
    (if (eqv? #!null super)
        '()
        (cons super direct-interfaces))))

;;; (all-superclasses a-class::Class)
;;; ---------------------------------------------------------------------
;;; returns a stably-ordered list of all superclasses and interfaces
;;; inherited by _a-class_.

(define (all-superclasses a-class::Class)
  (let ((direct-supers (direct-superclasses a-class)))
    (if (null? direct-supers)
        '()
        (remove-duplicates
         (append direct-supers
                 (apply append
                        (map all-superclasses
                             direct-supers)))))))

;;; =====================================================================
;;; generic functions
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; no-applicable-method
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

;;; ---------------------------------------------------------------------
;;; generic-function application
;;; ---------------------------------------------------------------------

;;; alists as method tables

;;; (make-method-entry type-list method-proc)
;;; ---------------------------------------------------------------------
;;; creates a method-entry, an internal structure used to store
;;; associations between type signatures and appliable methods in
;;; generic-function method tables

(define (make-method-entry type-list method-proc)
  (cons (copy-list type-list)
        method-proc))

;;; (method-entry-signature me)
;;; ---------------------------------------------------------------------
;;; returns the type signature from method-entry _me_

(define (method-entry-signature me)
  (car me))

;;; (method-entry-procedure me)
;;; ---------------------------------------------------------------------
;;; returns the method procedure from method-entry _me_

(define (method-entry-procedure me)
  (cdr me))

;;; (add-method type-list method-proc method-table)
;;; ---------------------------------------------------------------------
;;; adds a new method entry to _method-table_ associating _type-list_
;;; with _method-proc_ or, if an entry for _type-list_ already exists
;;; in _method-table_, replaces it

(define (add-method type-list method-proc method-table)
  (cons (make-method-entry type-list method-proc)
        method-table))

;;; (remove-method type-list method-table)
;;; ---------------------------------------------------------------------
;;; removes _type-list_ and its associated method procedure from
;;; _method-table_

(define (remove-method type-list method-table)
  (let loop ((entries method-table)
             (result '()))
    (if (null? entries)
        (reverse result)
        (if (equal? type-list (car entries))
            (loop (cdr entries)
                  result)
            (loop (cdr entries)
                  (cons (car entries)
                        result))))))

;;; (matching-types? type-list1 type-list2)
;;; ---------------------------------------------------------------------
;;; returns true if _type-list1_ is equal to _type-list2_, or if each
;;; of _type-list1_'s types is a subclass of the corresponding type in
;;; _type-list2_

(define (matching-types? type-list1 type-list2)
  (every? subclass? type-list1 type-list2))

;;; (more-specific-types? type-list1 type-list2)
;;; ---------------------------------------------------------------------
;;; returns true if all of the type objects in _type-list1_ are
;;; subclasses of the corresponding type objects in _type-list2_, but
;;; the reverse is not true. If this function returns true then
;;; _type-list1_ is said to be more specific than _type-list2_. klos
;;; always tries to apply the most specific applicable method to a
;;; given set of arguments.

(define (more-specific-types? type-list1 type-list2)
  (and (every? subclass? type-list1 type-list2)
       (every? (complement subclass?) type-list2 type-list1)))

;;; (find-matching-methods type-list method-table)
;;; ---------------------------------------------------------------------
;;; returns all methods whose type signatures in _method-table_ match the
;;; types in _type-list_. Type signatures match when the function
;;; matching-types? returns true.

(define (find-matching-methods type-list method-table)
  (let* ((argcount (length type-list))
         (right-length-entries (filter (lambda (m)(= argcount (length (car m))))
                                       method-table))
         (matching-entries (filter (lambda (m)(matching-types? type-list (car m)))
                                   method-table)))
    (sort matching-entries
          (lambda (x y)
            (more-specific-types? (method-entry-signature x)
                                  (method-entry-signature y))))))

;;; (most-specific-method type-list method-table)
;;; ---------------------------------------------------------------------
;;; returns the most specific method of _method-table_ that is
;;; applicable to the types in _type-list_

(define (most-specific-method type-list method-table)
  (let ((matching-methods (find-matching-methods type-list method-table)))
    (if (null? matching-methods)
        #f
        (method-entry-procedure (car matching-methods)))))

;;; (merge-method type-list method-proc method-table)
;;; ---------------------------------------------------------------------
;;; adds an association between _type-list_ and _method-proc_ in
;;; _method-table_, removing any previously existing entry for
;;; _type-list_

(define (merge-method type-list method-proc method-table)
  (add-method type-list
              method-proc
              (remove-method type-list method-table)))

;;; ---------------------------------------------------------------------
;;; generic functions
;;; ---------------------------------------------------------------------

;;; (%apply-gf-to-args methods args default-method)
;;; ---------------------------------------------------------------------
;;; a private function that executes generic function application.
;;; called when a generic function is applied to a set of arguments.

(define (%apply-gf-to-args methods args default-method)
  (let* ((arglist (array->list args))
         (argtypes (map (lambda (a)(a:getClass))
                        arglist))
         (matching-method (most-specific-method argtypes methods)))
    (if matching-method
        (apply matching-method arglist)
        (apply default-method arglist))))

;;; CLASS generic-function
;;; ---------------------------------------------------------------------
;;; the class of generic functions. generic functions are procedures
;;; that select a method to run by examining the types of arguments
;;; passed to them.

(define-simple-class generic-function (ProcedureN)
  (default-method::ProcedureN init-form: no-applicable-method)
  (methods init-form: '())
  ((getMethods) methods)
  ((applyN args::Object[]) (%apply-gf-to-args methods args default-method))
  ((addMethod argtypes::gnu.lists.LList method::ProcedureN)
   (set! methods (merge-method argtypes method methods))))

;;; (add-method! gf::generic-function type-list method-proc)
;;; ---------------------------------------------------------------------
;;; adds a new method _method-proc_ to generic function _gf_, creating
;;; an entry for _type-list_ in the generic function's internal
;;; method table

(define (add-method! gf::generic-function type-list method-proc)
  (*:addMethod gf type-list method-proc))

;;; MACRO (defgeneric name)
;;; ---------------------------------------------------------------------
;;; define a variable _gname_ whose value is a newly-constructed
;;; generic function

(define-syntax defgeneric
  (syntax-rules ()
    ((defgeneric gname) (define gname (generic-function)))))

;;; MACRO (defmethod (name . (param1 type1) ...) ...)
;;; ---------------------------------------------------------------------
;;; adds a new method to generic function _name_. The body of the
;;; macro form becomes the body of the method procedure. The parameter
;;; names given by param1... are the formal parameters of the method
;;; procedure and the generic function. The types given by type1...
;;; make up the type signature of the new method entry.

(define-syntax defmethod
  (syntax-rules ()
    ((defmethod mname ((var type) ...) expr ...)
     (add-method! mname (list type ...)
                  (lambda (var ...) (begin expr ...))))))

;;; ---------------------------------------------------------------------
;;; testing code
;;; ---------------------------------------------------------------------
;;; (defgeneric gadd)
;;; (defmethod gadd ((x gnu.math.IntNum) (y gnu.math.IntNum)) (+ x y))
;;; (gadd 2 3)
;;; (defmethod gadd ((x java.lang.String) (y java.lang.String)) (string-append x y))
;;; (gadd "Foo" "bar")
;;; (gadd "Foo" 3) ; should be an error: no applicable method
;;; (defgeneric what?)
;;; (defmethod what? ((x java.lang.Object)) 'object)
;;; (defmethod what? ((x java.lang.String)) 'string)
;;; (what? "foo")
;;; (what? (vector 1 2 3))
