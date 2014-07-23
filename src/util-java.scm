;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          methods.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       method-invocation conveniences
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export import-as jnull? defgetter defsetter)

(define (jnull? thing)
  (eqv? thing #!null))

(define-syntax import-as
  (syntax-rules ()
    ((import-as localname imported-name)
     (define-private-alias localname imported-name))))

(define-syntax defgetter
  (syntax-rules (::)
    ((defgetter (getter-name classname) method-name)
     (define (getter-name obj :: classname)
       (invoke obj 'method-name)))))

(define-syntax defsetter
  (syntax-rules (:: val)
    ((defsetter (setter-name classname) method-name)
     (define (setter-name obj :: classname val)
       (invoke obj 'method-name val)))))
