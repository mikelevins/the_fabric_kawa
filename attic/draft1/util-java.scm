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

(module-export import-as jnull?)

(define (jnull? thing)
  (eqv? thing #!null))

(define-syntax import-as
  (syntax-rules ()
    ((import-as localname imported-name)
     (define-private-alias localname imported-name))))
