;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          server.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       server main program
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export make-server)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(define-private-alias Context com.jme3.system.JmeContext)
(define-private-alias Network com.jme3.network.Network)
(define-private-alias SimpleApplication com.jme3.app.SimpleApplication)
(define-private-alias Server com.jme3.network.Server)

;;; ---------------------------------------------------------------------
;;; <fabric-server> - the server class
;;; ---------------------------------------------------------------------

(define-simple-class <fabric-server> (SimpleApplication)
  ;; slots
  ;; -------
  (network-listener::Server init-form: #!null)

  ;; accessors
  ;; ---------
  ((getNetworkListener) network-listener)
  ((setNetworkListener listener::Server) (set! network-listener listener))

  ;; methods
  ;; -------
  ((simpleInitApp) #!void)
  ((stopServer) (@ 'close network-listener)))


;;; ---------------------------------------------------------------------
;;; accessors
;;; ---------------------------------------------------------------------


;;; ---------------------------------------------------------------------
;;; initialization
;;; ---------------------------------------------------------------------

(define (make-server)
  (let* ((server (<fabric-server>)))
    server))

;;; ---------------------------------------------------------------------
;;; startup and shutdown
;;; ---------------------------------------------------------------------




