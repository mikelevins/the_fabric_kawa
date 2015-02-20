;;;; ***********************************************************************
;;;;
;;;; Name:          util-lists.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       utilities for working with lists
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 array->list
 choose-any
 copy-list
 every?
 every?
 get-key
 list-fill
 position-if
 put-key
 remove-duplicates
 shuffle
 some?)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; 

(require 'list-lib)
(require "util-random.scm")
(require "util-sort.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as Object java.lang.Object)

;;; ---------------------------------------------------------------------
;;; list utilities
;;; ---------------------------------------------------------------------

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (array->list arr::Object[])
  (let loop ((i 0)
             (result '()))
    (if (< i arr:length)
        (loop (+ i 1)
              (cons (arr i) result))
        (reverse result))))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (choose-any ls)
  (list-ref ls (random-integer (length ls))))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (copy-list ls)
  (map (lambda (x) x)
       ls))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (every? test ls #!rest (more-lists '()))
  (let loop ((lists (cons ls more-lists)))
    (if (some? null? lists)
        #t
        (let ((args (map car lists)))
          (if (apply test args)
              (loop (map cdr lists))
              #f)))))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (list-fill n thing)
  (let loop ((result '())
             (count 0))
    (if (< count n)
        (loop (cons thing result)
              (+ count 1))
        result)))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (position-if test ls)
  (let loop ((i 0)
             (items ls))
    (if (null? items)
        #f
        (if (test (car items))
            i
            (loop (+ 1 i)
                  (cdr items))))))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (remove-duplicates ls #!optional (test eq?))
  (let loop ((items ls)
             (result '()))
    (if (null? items)
        (reverse result)
        (let ((next (car items)))
          (if (some? (lambda (it)(test next it))
                     result)
              (loop (cdr items)
                    result)
              (loop (cdr items)
                    (cons next result)))))))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (shuffle ls)
  (sort ls (lambda (x y)(zero? (random-integer 2)))))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (some? test ls)
  (let loop ((items ls))
    (if (null? items)
        #f
        (if (test (car items))
            (car items)
            (loop (cdr items))))))

;;; ---------------------------------------------------------------------
;;; property lists
;;; ---------------------------------------------------------------------

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (get-key ls thing #!optional (default #f))
  (let ((mtail (member thing ls)))
    (if mtail
        (cadr mtail)
        default)))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (put-key ls key val)
  (let ((keypos (position-if (lambda (x)(equal? key x)) ls)))
    (if keypos
        (let ((head (take ls keypos))
              (tail (drop ls (+ keypos 2))))
          (append head
                  (cons key (cons val tail))))
        (cons key (cons val ls)))))
