;;;; ***********************************************************************
;;;;
;;;; Name:          util-java.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       java conveniences
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************
(format #t "~%loading util-java.cm")

(module-export
 context-class-loader
 current-thread
 get-resource
 import-as
 jnull?)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; utilities for interacting with the JVM from Kawa

;;; (context-class-loader thread::java.lang.Thread)
;;; ---------------------------------------------------------------------
;;; returns the Java class loader for _thread_

(define (context-class-loader thread::java.lang.Thread)
  (*:getContextClassLoader thread))

;;; (current-thread)
;;; ---------------------------------------------------------------------
;;; returns the thread on which the calling code is currently running

(define (current-thread)
  (java.lang.Thread:currentThread))

;;; (get-resource loader::java.lang.ClassLoader path::java.lang.String)
;;; ---------------------------------------------------------------------
;;; returns the resource data loaded by _loader_ from the file at _path_
;;; used for loading textures, fonts, and other Client media

(define (get-resource loader::java.lang.ClassLoader path::java.lang.String)
  (*:getResource loader path))


;;; (import-as local-name fully-qualified-name)
;;; ---------------------------------------------------------------------
;;; arranges for _local-name_ to be read as _fully-qualified-name_ in
;;; the current file. Used to provide more convenient aliases for
;;; long or cumbersome class and package names.

(define-syntax import-as
  (syntax-rules ()
    ((import-as localname imported-name)
     (define-private-alias localname imported-name))))

;;; (jnull? thing)
;;; ---------------------------------------------------------------------
;;; returns true if _thing_ is Java null

(define (jnull? thing)
  (eqv? thing #!null))


