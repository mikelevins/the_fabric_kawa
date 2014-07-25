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

(require "util-java.scm")
(require "language-types.scm")
(require "util-lists.scm")

(module-export generic-function
               no-applicable-method
               assert-method!
               defgeneric
               defmethod)

(import-as PersistentHashMap com.github.krukow.clj_lang.PersistentHashMap)
(import-as ProcedureN gnu.mapping.ProcedureN)

(define (no-applicable-method . args)
  (error (format #f "No applicable method for arguments ~s with types ~s "
                 args (map (lambda (a)(a:getClass))
                           args))))

(define (%apply-gf-to-args methods::PersistentHashMap args default-method)
  (let* ((arglist (array->list args))
         (argtypes (map (lambda (a)(a:getClass))
                        arglist))
         (matching-method (*:valAt methods argtypes #f)))
    (if matching-method
        (apply matching-method arglist)
        (apply default-method arglist))))

(define-simple-class generic-function (ProcedureN)
  (default-method::ProcedureN init-form: no-applicable-method)
  (methods::PersistentHashMap init-form: PersistentHashMap:EMPTY)
  ((applyN args::Object[]) (%apply-gf-to-args methods args default-method))
  ((addMethod argtypes::gnu.lists.LList method::ProcedureN) (set! methods (*:assoc methods argtypes method))))

(define (assert-method! gf::generic-function types::gnu.lists.LList method::ProcedureN)
  (*:addMethod gf types method))

;;; (define $gf (generic-function))
;;; (assert-method! $gf (list gnu.math.IntNum gnu.math.IntNum) (lambda (x y)(+ x y)))
;;; ($gf 2 3)

(define-syntax defgeneric
  (syntax-rules ()
    ((defgeneric gname) (define gname (generic-function)))))

;;; (defgeneric gadd)
;;; (assert-method! gadd (list gnu.math.IntNum gnu.math.IntNum) (lambda (x y)(+ x y)))
;;; (gadd 2 3)


(define-syntax defmethod
  (syntax-rules ()
    ((defmethod mname ((var type) ...) expr ...)
     (assert-method! mname (list type ...)
                     (lambda (var ...) (begin expr ...))))))


;;; (defgeneric gadd)
;;; (defmethod gadd ((x gnu.math.IntNum) (y gnu.math.IntNum)) (+ x y))
;;; (gadd 2 3)
;;; (defmethod gadd ((x gnu.math.IntNum) (y gnu.math.IntNum)(z gnu.math.IntNum)) (+ x y z))
;;; (gadd 2 3 4)
;;; (defmethod gadd ((x java.lang.String) (y java.lang.String)) (string-append x y))
;;; (gadd "Foo" "bar")


