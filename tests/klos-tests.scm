;;;; ***********************************************************************
;;;;
;;;; Name:          klos-tests.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       tests of the klos subsystem
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(require "../src/util-java.scm")
(require "../src/model-klos.scm")
(require "tests.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

;;; type-object?

(test-case type-object-1
           (type-object? 3)
           expecting: #f)

(test-case type-object-2
           (type-object? java.lang.Object)
           expecting: #t)

;;; subclass?

(test-case subclass-1
           (subclass? java.lang.Integer java.lang.Number)
           expecting: #t)

(test-case subclass-2
           (subclass? java.lang.String java.lang.Number)
           expecting: #f)

;;; get-class

(test-case get-class-1
           (get-class "Hello")
           expecting: java.lang.String)

;;; get-interfaces

(test-case get-interfaces-1
           (get-interfaces (get-class "Hello"))
           expecting:
           (list java.io.Serializable java.lang.Comparable
                 java.lang.CharSequence))

;;; direct-superclasses

(test-case direct-superclasses-1
           (direct-superclasses (get-class 5))
           expecting:
           (list gnu.math.RatNum java.io.Externalizable))

;;; all-superclasses

(test-case all-superclasses-1
           (all-superclasses (get-class 5))
           expecting:
           (list gnu.math.RatNum java.io.Externalizable gnu.math.RealNum
                 gnu.math.Complex java.lang.Comparable
                 gnu.math.Quaternion gnu.math.Quantity gnu.math.Numeric
                 java.lang.Number java.lang.Object java.io.Serializable))

(test-case all-superclasses-2
           (equal? (all-superclasses (get-class 5))
                   (all-superclasses (get-class 100)))
           expecting: #t)

(test-case all-superclasses-3
           (equal? (all-superclasses (get-class 5))
                   (all-superclasses (get-class 5.0)))
           expecting: #f)

;;; generic functions

(test-case generic-functions-1
           (begin (defgeneric test-add)
                  (defmethod test-add ((x java.lang.Object) (y java.lang.Object))
                    (cons x y))
                  (test-add 1 2))
           expecting: '(1 . 2))

(test-case generic-functions-2
           (begin (defgeneric test-add)
                  (defmethod test-add ((x java.lang.Object) (y java.lang.Object))
                    (cons x y))
                  (defmethod test-add ((x java.lang.Number) (y java.lang.Number))
                    (+ x y))
                  (test-add 1 2))
           expecting: 3)

(test-case generic-functions-3
           (begin (defgeneric test-add)
                  (defmethod test-add ((x java.lang.Object) (y java.lang.Object))
                    (cons x y))
                  (defmethod test-add ((x java.lang.Number) (y java.lang.Number))
                    (+ x y))
                  (defmethod test-add ((x java.lang.String) (y java.lang.String))
                    (string-append x y))
                  (list (test-add 'a 'b)
                        (test-add 1 2)
                        (test-add "foo" "bar")))
           expecting: '((a . b)
                        3
                        "foobar"))

