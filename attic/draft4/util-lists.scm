;;;; ***********************************************************************
;;;;
;;;; Name:          util-lists.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       utilities for working with lists
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************
(format #t "~%loading util-lists.cm")

(module-export
 any-n
 array->list
 choose-any
 copy-list
 every?
 every?
 get-key
 interpose
 list-fill
 position-if
 put-key
 remove-duplicates
 replace-element
 shuffle
 some?)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; 

(require 'list-lib)
(require "util-random.scm")
(require "util-sort.scm")
(require "util-java.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as Object java.lang.Object)

;;; (any-n n ls)
;;; ---------------------------------------------------------------------
;;; returns a list of n aribtrary elements of ls 

(define (any-n n ls)
  (take (shuffle ls) n))

;;; (array->list arr::Object[])
;;; ---------------------------------------------------------------------
;;; returns a list of the elements in _arr_ in the same order

(define (array->list arr::Object[])
  (let loop ((i 0)
             (result '()))
    (if (< i arr:length)
        (loop (+ i 1)
              (cons (arr i) result))
        (reverse result))))

;;; (choose-any ls)
;;; ---------------------------------------------------------------------
;;; returns an arbitrary element of ls

(define (choose-any ls)
  (list-ref ls (random-integer (length ls))))

;;; (copy-list ls)
;;; ---------------------------------------------------------------------
;;; returns a new list that contains the same elements as _ls_ in the
;;; same order

(define (copy-list ls)
  (map (lambda (x) x)
       ls))

;;; (every? test ls #!rest (more-lists '()))
;;; ---------------------------------------------------------------------
;;; returns a true value if applying _test_ to each element of _ls_
;;; returns a true value. If any element returns a false value, every?
;;; stops testing and immediately returns #f. If _ls_ is empty, every?
;;; returns #t

(define (every? test ls #!rest (more-lists '()))
  (let loop ((lists (cons ls more-lists)))
    (if (some? null? lists)
        #t
        (let ((args (map car lists)))
          (if (apply test args)
              (loop (map cdr lists))
              #f)))))


;;; (interpose cupola ls)
;;; ---------------------------------------------------------------------
;;; returns a new list in which _cupola_ has been inserted between
;;; each pair of elements of _ls_

(define (interpose cupola ls)
  (if (or (null? ls)
          (null? (cdr ls)))
      ls
      (cons (car ls)
            (cons cupola
                  (interpose cupola
                             (cdr ls))))))


;;; (list-fill n thing)
;;; ---------------------------------------------------------------------
;;; returns a list of _n_ elements, all equal to _thing_.

(define (list-fill n thing)
  (let loop ((result '())
             (count 0))
    (if (< count n)
        (loop (cons thing result)
              (+ count 1))
        result)))

;;; (position-if test ls)
;;; ---------------------------------------------------------------------
;;; applies _test_ to each element of _ls_. If any application returns
;;; a true value, position-if returns the index of the element that
;;; returned true.

(define (position-if test ls)
  (let loop ((i 0)
             (items ls))
    (if (null? items)
        #f
        (if (test (car items))
            i
            (loop (+ 1 i)
                  (cdr items))))))

;;; (remove-duplicates ls #!optional (test eq?))
;;; ---------------------------------------------------------------------
;;; returns a new list with the same elements as _ls_, except that
;;; duplicate elements in ls appear only once in the result. the
;;; function _test_ is used to determine whether two elements are the
;;; same; it must return a true value if two elements are to be
;;; considered duplicates, and #f otherwise.

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

;;; (replace-element ls index new-element)
;;; ---------------------------------------------------------------------
;;; returns a new list with the same elements as _ls_, except that
;;; the element at _index_ has been replaced with _new-element_

(define (replace-element ls index new-element)
  (if (<= (length ls) index)
      (error "Index out of range " index)
      (let ((head (take ls index))
            (tail (drop ls (+ 1 index))))
        (append head
                (cons new-element tail)))))

;;; (shuffle ls)
;;; ---------------------------------------------------------------------
;;; returns a copy of _ls_ in which the elements have been randomly
;;; rearranged.

(define (shuffle ls)
  (sort ls (lambda (x y)(zero? (random-integer 2)))))

;;; (some? test ls)
;;; ---------------------------------------------------------------------
;;; applies _test_ to each element of ls. If any such test returns a
;;; true value then some? stops testing and immediately returns that
;;; element of _ls_. If no such test returns true then some? returns
;;; #f. If_ls_ is empty then some? returns #f.

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
;;; a property list is a list of laternating keys and values. The
;;; first element is a key; the second is the value associated with
;;; that key; the third is another key; and so on


;;; (get-key ls key #!optional (default #f))
;;; ---------------------------------------------------------------------
;;; returns the value associated with _key_ in _ls_, or, if _key_
;;; isn't present in _ls_, returns _default_

(define (get-key ls key #!optional (default #f))
  (let ((mtail (member key ls)))
    (if mtail
        (cadr mtail)
        default)))


;;; (put-key ls key val)
;;; ---------------------------------------------------------------------
;;; returns a new copy of _ls_ in which the value _val_ appears as the
;;; value associated with the key _key_

(define (put-key ls key val)
  (let ((keypos (position-if (lambda (x)(equal? key x)) ls)))
    (if keypos
        (let ((head (take ls keypos))
              (tail (drop ls (+ keypos 2))))
          (append head
                  (cons key (cons val tail))))
        (cons key (cons val ls)))))
