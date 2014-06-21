;;;; ***********************************************************************
;;;;
;;;; Name:          generic-functions.scm
;;;; Project:       cloth: a CLOS-like object system
;;;; Purpose:       implementation of generic functions
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(define-private-alias PersistentHashMap com.github.krukow.clj_lang.PersistentHashMap)
(define-private-alias ProcedureN gnu.mapping.ProcedureN)

;;; ---------------------------------------------------------------------
;;; no-applicable-method
;;; ---------------------------------------------------------------------
;;; a generic function that we call when we can't find a matching
;;; method; we'll define the default method for it once generic
;;; functions are defined

(define no-applicable-method #f)

;;; ---------------------------------------------------------------------
;;; next-method
;;; ---------------------------------------------------------------------
;;; inside the body of an executing generic function, next-method is
;;; bound to the next applicable method for the current input parameters.
;;; at other times, it's meaningless.

(define next-method #f)

;;; ---------------------------------------------------------------------
;;; no-next-method
;;; ---------------------------------------------------------------------
;;; a generic function that next-method calls when there the body of a
;;; method calls next-method and there is no next-method. method;
;;; we'll define the default method for it once generic functions are
;;; defined.

(define no-next-method #f)

;;; ---------------------------------------------------------------------
;;; method-database
;;; ---------------------------------------------------------------------
;;; maps type signatures to methods in generic functions



;;; ---------------------------------------------------------------------
;;; generic-function dispatch
;;; ---------------------------------------------------------------------

(define (%order-methods method-list arglist)
  '())

(define (%matching-methods method-db arglist)
  '())

(define (compute-applicable-methods method-db args)
  (%order-methods (%matching-methods method-db args)
                  args))

(define (%apply-generic-function gf method-db args)
  (let ((applicable-methods (compute-applicable-methods method-db args)))
    (if (null? applicable-methods)
        (no-applicable-method gf args)
        (let ((effectivem (car applicable-methods))
              (nextm (if (null? (cdr applicable-methods))
                         '()
                         (cadr applicable-methods))))
          (fluid-let ((next-method nextm))
            (apply effectivem args))))))

;;; ---------------------------------------------------------------------
;;; the generic-function class
;;; ---------------------------------------------------------------------

(define-simple-class generic-function (ProcedureN)
  (method-db init-form: #f)
  ((applyN args) (%apply-generic-function (this) method-db args))
  ((addMethod type-signature method) (%assert-method! method-db type-signature method))
  ((removeMethod type-signature) (%retract-method! method-db type-signature)))

