;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          store.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the world store
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export)

(require "util-java.scm")
(require "init-config.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(define-private-alias HyperGraph org.hypergraphdb.HyperGraph)
(define-private-alias Runnable java.lang.Runnable)
(define-private-alias Runtime java.lang.Runtime)
(define-private-alias Thread java.lang.Thread)


;;; ---------------------------------------------------------------------
;;; store parameters
;;; ---------------------------------------------------------------------

(define world-store (make-parameter #!null))

;;; ---------------------------------------------------------------------
;;; startup and shutdown
;;; ---------------------------------------------------------------------

(define (shutdown-world-store)
  (let ((ws::HyperGraph (world-store)))
    (unless (eq? #!null ws)
      (@ 'close ws)
      (world-store #!null))))

(define-simple-class StoreCloser (Runnable)
  ((run) (shutdown-world-store)))

;;; open the world database, configure it, and add a Java shutdown
;;; hook to make sure it gets closed
(define (init-world-store)
  (world-store (HyperGraph (string-append (database-root) "world.db/")))
  (let ((closer::Runnable (StoreCloser))
        (runtime::Runtime (Runtime:getRuntime)))
    (@ 'addShutdownHook runtime (Thread closer))))

;;; (init-world-store)
;;; (world-store)
;;; (shutdown-world-store)

