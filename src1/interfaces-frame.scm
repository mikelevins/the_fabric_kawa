;;;; ***********************************************************************
;;;;
;;;; Name:          interfaces-frame.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the Frame interface
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export IFrame get-key put-key remove-key contains-key? list-keys empty-frame?
               IMutableFrame set-key! delete-key!)

;;; ---------------------------------------------------------------------
;;; immutable IFrame
;;; ---------------------------------------------------------------------

(define-simple-class IFrame () interface: #t
  ((getKey key) #!abstract)
  ((putKey key val) #!abstract)
  ((removeKey key) #!abstract)
  ((containsKey key) #!abstract)
  ((listKeys) #!abstract)
  ((isEmpty) #!abstract))

(define (get-key fr key #!optional (default #f))
  (if (*:containsKey fr key)
      (*:getKey fr key)
      default))

(define (put-key fr key val)(*:putKey fr key val))
(define (remove-key fr key)(*:removeKey fr key))
(define (contains-key? fr key)(*:containsKey fr key))
(define (list-keys fr)(*:listKeys fr))
(define (empty-frame? fr)(*:isEmpty fr))

;;; ---------------------------------------------------------------------
;;; mutable IMutableFrame
;;; ---------------------------------------------------------------------

(define-simple-class IMutableFrame (IFrame) interface: #t
  ((setKey key val) #!abstract)
  ((deleteKey key) #!abstract))

(define (set-key! fr key val)
  (*:setKey fr key val))

(define (delete-key! fr key)
  (*:deleteKey fr key))

