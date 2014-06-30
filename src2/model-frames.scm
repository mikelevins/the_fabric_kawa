;;;; ***********************************************************************
;;;; Name:          model-pmap.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       simple property maps
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export Frame frame? frame empty-frame? contains-key? contains-val?
               get-key put-key set-key! remove-key delete-key! merge-keys frame-keys frame-vals)

(require 'list-lib)
(require "utilities-lists.scm")

(define-class Frame ()
  (slots init-form: '())
  ;; ---------
  ((frameSlots) slots)
  ((setFrameSlots new-slots) (set! slots new-slots)))

(define (frame? x)
  (instance? x Frame))

(define (frame . key/value-plist)
  (let ((slots (plist->alist key/value-plist))
        (fr (Frame)))
    (*:setFrameSlots fr slots)
    fr))

(define (empty-frame? thing)
  (and (frame? thing)
       (null? (*:frameSlots thing))))

(define (contains-key? fr :: Frame key)
  (any (lambda (slot)(equal? key (car slot)))
       (*:frameSlots fr)))

(define (contains-val? fr :: Frame val)
  (any (lambda (slot)(equal? val (cdr slot)))
       (*:frameSlots fr)))

(define (get-key fr :: Frame key #!optional (default #f))
  (let ((slot (assoc key (*:frameSlots fr))))
    (if slot
        (cdr slot)
        default)))

(define (put-key fr :: Frame key val)
  (let ((new-frame (Frame)))
    (*:setFrameSlots new-frame
                     (cons (key val)
                           (remove (lambda (slot)(equal? key (car slot)))
                                   (*:frameSlots fr))))
    new-frame))

(define (set-key! fr :: Frame key val)
  (let ((slot (assoc key (*:frameSlots fr))))
    (if slot
        (set-cdr! slot val)
        (*:setFrameSlots fr
                         (cons (cons key val)
                               (*:frameSlots fr))))))

(define (remove-key fr :: Frame key)
  (let ((new-frame (Frame)))
    (*:setFrameSlots new-frame
                     (remove (lambda (slot)(equal? key (car slot)))
                             (*:frameSlots fr)))
    new-frame))

(define (delete-key! fr :: Frame key)
  (*:setFrameSlots fr
                   (remove (lambda (slot)(equal? key (car slot)))
                           (*:frameSlots fr)))
  fr)

(define (frame-keys fr :: Frame)
  (map car (*:frameSlots fr)))

(define (frame-vals fr :: Frame)
  (map cdr (*:frameSlots fr)))

(define (merge-keys f1 :: Frame f2 :: Frame)
  (let loop ((ks (frame-keys f2))
	     (f* f1))
    (if (null? ks)
	f*
	(let* ((k (car ks))
	       (v (get-key f2 k)))
	  (loop (cdr ks)
                (put-key f* k v))))))

