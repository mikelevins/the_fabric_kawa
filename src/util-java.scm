;;;; ***********************************************************************
;;;;
;;;; Name:          util-java.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       java conveniences
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export current-thread context-class-loader get-resource import-as jnull?)

(define (context-class-loader thread::java.lang.Thread)
  (*:getContextClassLoader thread))

(define (current-thread)
  (java.lang.Thread:currentThread))

(define (get-resource loader::java.lang.ClassLoader path::java.lang.String)
  (*:getResource loader path))

(define-syntax import-as
  (syntax-rules ()
    ((import-as localname imported-name)
     (define-private-alias localname imported-name))))

(define (jnull? thing)
  (eqv? thing #!null))


