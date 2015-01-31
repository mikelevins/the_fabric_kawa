;;;; ***********************************************************************
;;;;
;;;; Name:          model-entity.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       representation of game entities
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export entity entity? entity-properties entity-type)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; an entity is represented as a cons cell whose car is a symbol
;;; that names the entity type, and whose cdr is a property list
;;; that represents the entity's properties

;;; ---------------------------------------------------------------------
;;; 
;;; ---------------------------------------------------------------------

(define (entity entity-type . properties)
  (cons entity-type properties))

(define (entity? thing)
  (and (pair? thing)
       (symbol? (car thing))))

(define (entity-type thing)
  (car thing))

(define (entity-properties thing)
  (cdr thing))


