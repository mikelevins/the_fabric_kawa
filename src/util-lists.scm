;;;; ***********************************************************************
;;;;
;;;; Name:          util-lists.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       utilities for working with lists
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export array->list every? copy-list drop every? filter get-key
               position-if put-key remove-duplicates some? take)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as Object java.lang.Object)

;;; ---------------------------------------------------------------------
;;; list utilities
;;; ---------------------------------------------------------------------

(define (array->list arr::Object[])
  (let loop ((i 0)
             (result '()))
    (if (< i arr:length)
        (loop (+ i 1)
              (cons (arr i) result))
        (reverse result))))

(define (copy-list ls)
  (map (lambda (x) x)
       ls))

(define (drop n ls)
  (let loop ((i n)
             (items ls))
    (if (<= i 0)
        items
        (if (null? items)
            (error "Index out of range" n)
            (loop (- i 1)
                  (cdr items))))))

(define (every? test ls #!rest (more-lists '()))
  (let loop ((lists (cons ls more-lists)))
    (if (some? null? lists)
        #t
        (let ((args (map car lists)))
          (if (apply test args)
              (loop (map cdr lists))
              #f)))))

(define (filter test ls)
  (let loop ((items ls)
             (result '()))
    (if (null? items)
        (reverse result)
        (if (test (car items))
            (loop (cdr items)
                  (cons (car items)
                        result))
            (loop (cdr items)
                  result)))))

(define (position-if test ls)
  (let loop ((i 0)
             (items ls))
    (if (null? items)
        #f
        (if (test (car items))
            i
            (loop (+ 1 i)
                  (cdr items))))))

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

(define (some? test ls)
  (let loop ((items ls))
    (if (null? items)
        #f
        (if (test (car items))
            (car items)
            (loop (cdr items))))))

(define (take n ls)
  (let loop ((i n)
             (items ls)
             (result '()))
    (if (<= i 0)
        (reverse result)
        (if (null? items)
            (error "Index out of range" n)
            (loop (- i 1)
                  (cdr items)
                  (cons (car items)
                        result))))))

;;; ---------------------------------------------------------------------
;;; property lists
;;; ---------------------------------------------------------------------

(define (get-key ls thing #!optional (default #f))
  (let ((mtail (member thing ls)))
    (if mtail
        (cadr mtail)
        default)))

(define (put-key ls key val)
  (let ((keypos (position-if (lambda (x)(equal? key x)) ls)))
    (if keypos
        (let ((head (take keypos ls))
              (tail (drop (+ keypos 2) ls)))
          (append head
                  (cons key (cons val tail))))
        (cons key (cons val ls)))))
