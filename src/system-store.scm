;;;; ***********************************************************************
;;;;
;;;; Name:          system-store.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       interface to the object store
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export close-fabric-store fabric-store fabric-store-path
               get-root-object open-fabric-store set-root-object!)

(require "util-java.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as Storage org.garret.perst.Storage)
(import-as StorageFactory org.garret.perst.StorageFactory)
(import-as String java.lang.String)

;;; ---------------------------------------------------------------------
;;; store parameters
;;; ---------------------------------------------------------------------

(define fabric-store (make-parameter #f))
(define fabric-store-path (make-parameter "/Users/mikel/Desktop/fabricStore"))

(define (open-fabric-store)
  (let ((db (*:createStorage (StorageFactory:getInstance)))
        (path::String (fabric-store-path)))
    (*:open db path)
    (fabric-store db)
    db))

(define (close-fabric-store)
  (let ((store::Storage (fabric-store)))
    (*:close store)
    (fabric-store #f)))

(define (get-root-object store)
  (*:getRoot store))

(define (set-root-object! store val)
  (*:setRoot store val))

;;; (fabric-store)
;;; (open-fabric-store)
;;; (close-fabric-store)
;;; (get-root-object (fabric-store))
;;; (set-root-object! (fabric-store) (list 'a 'b 'c))
