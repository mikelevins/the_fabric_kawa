;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          language-types.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       types and linearization for gf
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export class-of subclass? get-interfaces direct-superclasses all-superclasses)

(require 'list-lib)
(require "util-general.scm")
(require "util-java.scm")
(require "util-lists.scm")

(import-as Class java.lang.Class)

;;; ---------------------------------------------------------------------
;;; type utilities
;;; ---------------------------------------------------------------------

(define (class-of thing)
  (*:getClass thing))

;;; ---------------------------------------------------------------------
;;; obtaining stably-sorted lists of superclasses
;;; ---------------------------------------------------------------------

(define (subclass? class1::Class class2::Class)
  (if (*:isAssignableFrom class2 class1)
      #t
      #f))

(define (get-interfaces a-class::Class)
  (gnu.lists.LList:makeList (*:getInterfaces a-class) 0))

(define (direct-superclasses a-class::Class)
  (let* ((super (*:getSuperclass a-class))
         (direct-interfaces (get-interfaces a-class))
         (super-interfaces (if (eq? #!null super)
                               '()
                               (get-interfaces super))))
    (let loop1 ((supers (list a-class))
                (directs direct-interfaces))
      (if (null? directs)
          (let loop2 ((supers (if (eq? #!null super)
                                  supers
                                  (cons super supers)))
                      (indirects super-interfaces))
            (if (null? indirects)
                (reverse supers)
                (let ((ind (car indirects))
                      (more (cdr indirects)))
                  (loop2 (if (member ind supers)
                             supers
                             (cons ind supers))
                         (cdr indirects)))))
          (loop1 (cons (car directs)
                       supers)
                 (cdr directs))))))

(define (all-superclasses cl::Class)
  (let ((direct-supers (cdr (direct-superclasses cl))))
    (if (null? direct-supers)
        (list cl)
        (let ((cpls (map all-superclasses direct-supers)))
          (cons cl (apply append cpls))))))


