;;;; ***********************************************************************
;;;;
;;;; Name:          system-store.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       interface to the object store
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export close-store get-root-object open-store set-root-object!)

(require "server-config.scm")
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

(define (open-store path::String)
  (let ((db::Storage (*:createStorage (StorageFactory:getInstance))))
    (*:open db path)
    db))

(define (close-store store::Storage)
  (*:close store))

(define (get-root-object store::Storage)
  (*:getRoot store))

(define (set-root-object! store::Storage val)
  (*:setRoot store val))

