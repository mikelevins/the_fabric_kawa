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

;;; (entity entity-type . properties)
;;; ---------------------------------------------------------------------
;;; returns a new entity of type _entity-type_ (a symbol), and with
;;; properties _properties_ (a plist whose keys are keywords)

(define (entity entity-type . properties)
  (cons entity-type properties))

;;; (entity? thing)
;;; ---------------------------------------------------------------------
;;; returns true if _thing_ is an entity

(define (entity? thing)
  (and (pair? thing)
       (symbol? (car thing))))

;;; (entity-type thing)
;;; ---------------------------------------------------------------------
;;; returns the entity type of _thing_. the entity type is a symbol

(define (entity-type thing)
  (car thing))

;;; (entity-type? thing type)
;;; ---------------------------------------------------------------------
;;; returns true if _thing_'s entity type is _type_ (a symbol)

(define (entity-type? thing type)
  (eqv? type (entity-type thing)))

;;; (entity-properties thing)
;;; ---------------------------------------------------------------------
;;; returns the entity's properties as a plist whose keys are keywords

(define (entity-properties thing)
  (cdr thing))

;;; (get-property thing property #!key (default #f))
;;; ---------------------------------------------------------------------
;;; returns the property value of _property_ in _thing_, if present
;;; if the _property_ is not present in _thing_, returns _default_

(define (get-property thing property #!key (default #f))
  (let* ((props (entity-properties thing))
         (tail (memq property props)))
    (if tail
        (cadr tail)
        default)))

;;; (put-property thing property val)
;;; ---------------------------------------------------------------------
;;; returns a new entity which is a copy of _thing_, but with the
;;; property _property_ having the value _val_

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
