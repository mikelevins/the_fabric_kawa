;;;; ***********************************************************************
;;;;
;;;; Name:          model-entity.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       representation of game entities
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export entity entity? entity-properties entity-type entity-type?
               get-property put-property)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; an entity is represented as a cons cell whose car is a symbol
;;; that names the entity type, and whose cdr is a property list
;;; that represents the entity's properties

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-lists.scm")

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

(define (entity-type? thing type)
  (eqv? type (entity-type thing)))

(define (entity-properties thing)
  (cdr thing))

(define (get-property thing property #!key (default #f))
  (let* ((props (entity-properties thing))
         (tail (memq property props)))
    (if tail
        (cadr tail)
        default)))

(define (put-property thing property val)
  (let ((keypos (position-if (lambda (it)(eqv? property it)) thing)))
    (if keypos
        (let ((head (take keypos thing))
              (tail (drop (+ 2 keypos) thing)))
          (append head
                  (cons property
                        (cons val tail))))
        (cons (car thing)
              (cons property
                    (cons val
                          (cdr thing)))))))
