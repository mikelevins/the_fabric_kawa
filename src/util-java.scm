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

(module-export @ jnull?)


(define (@ method-name object . args)
  (apply invoke object method-name args))

(define (jnull? thing)
  (eqv? thing #!null))
