;;;; ***********************************************************************
;;;;
;;;; Name:          util-lists.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       utilities for working with lists
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export array->list every? copy-list every? filter getf remove-duplicates some?)

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

(define (getf ls thing #!optional (default #f))
  (let ((mtail (member thing ls)))
    (if mtail
        (cadr mtail)
        default)))

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

