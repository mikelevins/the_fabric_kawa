;;;; ***********************************************************************
;;;;
;;;; Name:          util-java.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       java conveniences
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
