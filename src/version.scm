;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          version.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the current fabric version
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 fabric-major-version
 fabric-minor-version
 fabric-patch-version
 fabric-version
 fabric-version-string)

(define fabric-version (make-parameter (vector 0 2 383)))

(define (fabric-major-version)
  (vector-ref (fabric-version) 0))

(define (fabric-minor-version)
  (vector-ref (fabric-version) 1))

(define (fabric-patch-version)
  (vector-ref (fabric-version) 2))

(define (fabric-version-string)
  (format #f "~A.~A.~A"
          (fabric-major-version)
          (fabric-minor-version)
          (fabric-patch-version)))


