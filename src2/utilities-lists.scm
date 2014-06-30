;;;; ***********************************************************************
;;;; Name:          utilities-lists.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       general-purpose utilities
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export plist->alist copy-tree)

(require 'list-lib)

(define (plist->alist plist)
  (let loop ((kvs plist))
    (if (null? kvs)
        '()
        (if (null? (cdr kvs))
            (error "malformed property list" plist)
            (cons (cons (car kvs)
                        (cadr kvs))
                  (plist->alist (cddr kvs)))))))

(define (copy-tree ls)
  (if (null? ls)
      '()
      (cons (car ls)
            (copy-tree (cdr ls)))))
