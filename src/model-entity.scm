;;;; ***********************************************************************
;;;;
;;;; Name:          model-entity.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       representation of game entities
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 entity
 entity-properties
 entity-type
 entity-type?
 entity?
 get-property
 put-property)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; This file implements an extremely simple form of an entity data
;;; structure. Entities are flexibile, composable structure used to
;;; represent game objects with flexible schemas. An entity is
;;; represented as a list whose head is a symbol that names the
;;; entity type, and whose tail is a property list representing the
;;; entity's properties.
;;;
;;; It's best to pretend you don't know that entities are lists and
;;; avoid using list functions on them. Instead, use (and extend) the
;;; APIs in this file. That way if we need to change the
;;; representation of entities for some reason in the future, we can
;;; do so with minimal impact on the rest of the code.

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require 'list-lib)
(require "util-lists.scm")

;;; ---------------------------------------------------------------------
;;; 
;;; ---------------------------------------------------------------------

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (entity entity-type . properties)
  (cons entity-type properties))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (entity? thing)
  (and (pair? thing)
       (symbol? (car thing))))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (entity-type thing)
  (car thing))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (entity-type? thing type)
  (eqv? type (entity-type thing)))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (entity-properties thing)
  (cdr thing))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (get-property thing property #!key (default #f))
  (let* ((props (entity-properties thing))
         (tail (memq property props)))
    (if tail
        (cadr tail)
        default)))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (put-property thing property val)
  (let ((keypos (position-if (lambda (it)(eqv? property it)) thing)))
    (if keypos
        (let ((head (take thing keypos))
              (tail (drop thing (+ 2 keypos))))
          (append head
                  (cons property
                        (cons val tail))))
        (cons (car thing)
              (cons property
                    (cons val
                          (cdr thing)))))))
