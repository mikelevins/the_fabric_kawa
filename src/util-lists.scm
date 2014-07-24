;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          util.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       general-purpose utilities
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export ArrayList->list list->ArrayList array->list list->array
               some? choose-any drop-any interleave list-fill shuffle
               plist->alist alist->plist alist alist-keys alist-vals
               adjoin-all position-if getf)

(require 'list-lib)
(require 'srfi-95) ; sorting
(require "util-java.scm")
(require "util-random.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as ArrayList java.util.ArrayList)
(import-as Class java.lang.Class)
(import-as Map com.github.krukow.clj_lang.PersistentHashMap)
(import-as Thread java.lang.Thread)
(import-as UUID java.util.UUID)

;;; ---------------------------------------------------------------------
;;; utils
;;; ---------------------------------------------------------------------

(define (adjoin-all pred ls items)
  (let loop ((result ls)
             (its items))
    (if (null? its)
        result
        (loop (lset-adjoin pred result (car its))
              (cdr its)))))

(define (ArrayList->list al::ArrayList)
  (gnu.lists.LList:makeList al))

(define (list->ArrayList instance::gnu.lists.LList)
  (ArrayList instance))

(define (array->list arr::java.lang.Object[])
  (let loop ((i 0)
             (result '()))
    (if (< i arr:length)
        (loop (+ i 1)
              (cons (arr i) result))
        (reverse result))))

(define (list->array ls)
  (apply java.lang.Object[] ls))

(define (some? predicate ls)
  (if (null? ls)
      #f
      (if (predicate (car ls))
          (car ls)
          (some? predicate (cdr ls)))))

(define (choose-any ls)
  (list-ref ls (random-integer (length ls))))

(define (drop-any ls)
  (let ((i (random-integer (length ls))))
    (append (take ls i)
            (drop ls (+ i 1)))))

(define (interleave left-list right-list)
  (let loop ((l left-list)
             (r right-list))
    (if (or (null? l)
            (null? r))
        '()
        (cons (car l)
              (cons (car r)
                    (interleave (cdr l)
                                (cdr r)))))))

(define (list-fill n thing)
  (let loop ((result '())
             (count 0))
    (if (< count n)
        (loop (cons thing result)
              (+ count 1))
        result)))

(define (shuffle ls)
  (sort ls (lambda (x y)(zero? (random-integer 2)))))

(define (plist->alist plist)
  (let loop ((kvs plist))
    (if (null? kvs)
        '()
        (if (null? (cdr kvs))
            (error "malformed property list" plist)
            (cons (cons (car kvs)
                        (cadr kvs))
                  (plist->alist (cddr kvs)))))))

(define (alist->plist alist)
  (let loop ((pairs alist))
    (if (null? pairs)
        '()
        (let ((pair (car pairs)))
          (cons (car pair)
                (cons (cdr pair)
                      (alist->plist (cdr pairs))))))))

(define (alist . plist)
  (plist->alist plist))

(define (alist-keys alist)
  (map car alist))

(define (alist-vals alist)
  (map cdr alist))

(define (position-if pred ls)
  (let loop ((tail ls)
             (i 0))
    (if (null? tail)
        #f
        (if (pred (car tail))
            i
            (loop (cdr tail) (+ i 1))))))

(define (getf ls thing #!optional (default #f))
  (let ((mtail (member thing ls)))
    (if mtail
        (cadr mtail)
        default)))
