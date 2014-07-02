;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          language-gf.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       generic functions
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(require "language-types.scm")
(require "utilities-lists.scm")

(module-export GenericFunction
               no-applicable-method
               assert-method!
               defgeneric
               defmethod)


(define-private-alias PersistentHashMap com.github.krukow.clj_lang.PersistentHashMap)
(define-private-alias ProcedureN gnu.mapping.ProcedureN)

(define (no-applicable-method . args)
  (error (format #f "No applicable method for arguments ~s with types ~s "
                 args (map (lambda (a)(a:getClass))
                           args))))

(define (%type-for-gf-dispatch thing)
  (let ((maybe-singleton (find-singleton thing)))
    (or maybe-singleton
        (thing:getClass))))

(define (%apply-gf-to-args methods::PersistentHashMap args default-method)
  (let* ((arglist (array->list args))
         (argtypes (map (lambda (a)(%type-for-gf-dispatch a))
                        arglist))
         (matching-method (*:valAt methods argtypes #f)))
    (if matching-method
        (apply matching-method arglist)
        (apply default-method arglist))))

(define-simple-class GenericFunction (ProcedureN)
  (default-method::ProcedureN init-form: no-applicable-method)
  (methods::PersistentHashMap init-form: PersistentHashMap:EMPTY)
  ((applyN args::Object[]) (%apply-gf-to-args methods args default-method))
  ((addMethod argtypes::gnu.lists.LList method::ProcedureN) (set! methods (*:assoc methods argtypes method))))

(define (assert-method! gf::GenericFunction types::gnu.lists.LList method::ProcedureN)
  (*:addMethod gf types method))

;;; (define $gf (GenericFunction))
;;; (assert-method! $gf (list gnu.math.IntNum gnu.math.IntNum) (lambda (x y)(+ x y)))
;;; ($gf 2 3)

(define-syntax defgeneric
  (syntax-rules ()
    ((defgeneric gname) (define gname (GenericFunction)))))

;;; (defgeneric gadd)
;;; (assert-method! gadd (list gnu.math.IntNum gnu.math.IntNum) (lambda (x y)(+ x y)))
;;; (gadd 2 3)

(define-syntax defmethod
  (syntax-rules ()
    ((defmethod (mname (var type) ...) expr ...)
     (assert-method! mname (list type ...)
                     (lambda (var ...) (begin expr ...))))))


;;; (defgeneric gadd)
;;; (defmethod (gadd a) (list a))
;;; (defmethod gadd ((x gnu.math.IntNum) (y gnu.math.IntNum)) (+ x y))
;;; (gadd 2 3)
;;; (defmethod gadd ((x gnu.math.IntNum) (y gnu.math.IntNum)(z gnu.math.IntNum)) (+ x y z))
;;; (gadd 2 3 4)
;;; (defmethod gadd ((x java.lang.String) (y java.lang.String)) (string-append x y))
;;; (gadd "Foo" "bar")
;;; (defmethod gadd ((x java.lang.String) (y (singleton 1))) "Wait a minute!")
;;; (gadd "Foo" 1)



