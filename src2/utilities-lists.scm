;;;; ***********************************************************************
;;;; Name:          utilities-lists.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       general-purpose utilities
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export adjoin-all array->list copy-tree plist->alist)

(require 'list-lib)

(define (adjoin-all pred ls items)
  (let loop ((result ls)
             (its items))
    (if (null? its)
        result
        (loop (lset-adjoin pred result (car its))
              (cdr its)))))

(define (array->list arr::java.lang.Object[])
  (let loop ((i 0)
             (result '()))
    (if (< i arr:length)
        (loop (+ i 1)
              (cons (arr i) result))
        (reverse result))))

(define (copy-tree ls)
  (if (null? ls)
      '()
      (cons (car ls)
            (copy-tree (cdr ls)))))

(define (plist->alist plist)
  (let loop ((kvs plist))
    (if (null? kvs)
        '()
        (if (null? (cdr kvs))
            (error "malformed property list" plist)
            (cons (cons (car kvs)
                        (cadr kvs))
                  (plist->alist (cddr kvs)))))))
