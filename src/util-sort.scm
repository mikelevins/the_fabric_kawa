;;;; ***********************************************************************
;;;;
;;;; Name:          util-sort.scm
;;;; Project:       the Fabric
;;;; Purpose:       sort function from Gambit
;;;; Author:        Marc Feeley
;;;; Copyright:     2006-2008 by Marc Feeley, All Rights Reserved.
;;;;
;;;; ***********************************************************************
;;;; slightly modified by mikel evins

(module-export sort)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; the sort routine in this file was written by Marc Feeley and is
;;; distributed as part of the Gambit-C Scheme system. It has been
;;; very lightly modified by mikel evins to work with the Fabric code.

;;; (sort sequence less?)
;;; ---------------------------------------------------------------------
;;; returns a new copy of _sequence_ whose elements have been sorted
;;; in ascending order according to the comparison function _less?_

(define (sort sequence less?)
  (define (sort-list lst less?)
    (define (mergesort lst)
      (define (merge lst1 lst2)
        (cond ((not (pair? lst1))
               lst2)
              ((not (pair? lst2))
               lst1)
              (else
               (let ((e1 (car lst1)) (e2 (car lst2)))
                 (if (less? e1 e2)
                     (cons e1 (merge (cdr lst1) lst2))
                     (cons e2 (merge lst1 (cdr lst2))))))))
      (define (split lst)
        (if (or (not (pair? lst)) (not (pair? (cdr lst))))
            lst
            (cons (car lst) (split (cddr lst)))))
      (if (or (not (pair? lst)) (not (pair? (cdr lst))))
          lst
          (let* ((lst1 (mergesort (split lst)))
                 (lst2 (mergesort (split (cdr lst)))))
            (merge lst1 lst2))))
    (mergesort lst))
  (cond ((not (procedure? less?))
         (error "procedure expected"))
        ((or (null? sequence)
             (pair? sequence))
         (sort-list sequence less?))
        ((vector? sequence)
         (list->vector (sort-list (vector->list sequence) less?)))
        (else
         (error "vector or list expected"))))
