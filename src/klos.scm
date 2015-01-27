;;;; ***********************************************************************
;;;;
;;;; Name:          klos.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       generic functions for Kawa
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export type-object?)

(require "sort.scm")
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

(define (type-object? thing)
  (instance? thing Type))

(define (subclass? tp1::Class tp2::Class)
  (*:isAssignableFrom tp2 tp1))

(define (get-class thing::Object)
  (*:getClass thing))

(define (get-interfaces a-class::Class)
  (gnu.lists.LList:makeList (*:getInterfaces a-class) 0))

(define (direct-superclasses a-class::Class)
  (let* ((super (*:getSuperclass a-class))
         (supers (if (eqv? #!null super)
                     '()
                     (list super)))
         (direct-interfaces (get-interfaces a-class)))
    (if (eqv? #!null super)
        '()
        (cons super direct-interfaces))))

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

(define (no-applicable-method . args)
  (error (format #f "No applicable method for arguments ~s with types ~s "
                 args (map (lambda (a)(a:getClass))
                           args))))

;;; ---------------------------------------------------------------------
;;; generic-function application
;;; ---------------------------------------------------------------------

;;; alists as method tables

(define (make-method-entry type-list method-proc)
  (cons (copy-list type-list)
        method-proc))

(define (method-entry-signature me)
  (car me))

(define (method-entry-procedure me)
  (cdr me))

(define (add-method type-list method-proc method-table)
  (cons (make-method-entry type-list method-proc)
        method-table))

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

(define (matching-types? type-list1 type-list2)
  (every? subclass? type-list1 type-list2))

(define (more-specific-types? type-list1 type-list2)
  (and (every? subclass? type-list1 type-list2)
       (every? (complement subclass?) type-list1 type-list2)))

(define (find-matching-methods type-list method-table)
  (let* ((argcount (length type-list))
         (right-length-entries (filter (lambda (m)(= argcount (length (car m))))
                                       method-table))
         (matching-entries (filter (lambda (m)(matching-types? (car m)
                                                               type-list))
                                   method-table)))
    (sort matching-entries
          (lambda (x y)
            (more-specific-types? (method-entry-signature x)
                                  (method-entry-signature y))))))

(define (most-specific-method type-list method-table)
  (let ((matching-methods (find-matching-methods type-list method-table)))
    (if (null? matching-methods)
        #f
        (cdr (car matching-methods)))))

(define (merge-method type-list method-proc method-table)
  (add-method type-list
              method-proc
              (remove-method type-list method-table)))

;;; generic functions

(define (%apply-gf-to-args methods args default-method)
  (let* ((arglist (array->list args))
         (argtypes (map (lambda (a)(a:getClass))
                        arglist))
         (matching-method (most-specific-method argtypes methods)))
    (if matching-method
        (apply matching-method arglist)
        (apply default-method arglist))))

(define-simple-class generic-function (ProcedureN)
  (default-method::ProcedureN init-form: no-applicable-method)
  (methods init-form: '())
  ((getMethods) methods)
  ((applyN args::Object[]) (%apply-gf-to-args methods args default-method))
  ((addMethod argtypes::gnu.lists.LList method::ProcedureN)
   (set! methods (merge-method argtypes method methods))))

(define (add-method! gf::generic-function type-list method-proc)
  (*:addMethod gf type-list method-proc))

(define-syntax defgeneric
  (syntax-rules ()
    ((defgeneric gname) (define gname (generic-function)))))

(define-syntax defmethod
  (syntax-rules ()
    ((defmethod mname ((var type) ...) expr ...)
     (add-method! mname (list type ...)
                  (lambda (var ...) (begin expr ...))))))

;;; (defgeneric gadd)
;;; (defmethod gadd ((x gnu.math.IntNum) (y gnu.math.IntNum)) (+ x y))
;;; (gadd 2 3)
;;; (defmethod gadd ((x java.lang.String) (y java.lang.String)) (string-append x y))
;;; (gadd "Foo" "bar")
;;; (gadd "Foo" 3) ; should be error: no applicable method

