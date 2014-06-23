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

(module-export server-port server-address make-server start-server stop-server)


;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(define-private-alias Context com.jme3.system.JmeContext)
(define-private-alias Network com.jme3.network.Network)
(define-private-alias SimpleApplication com.jme3.app.SimpleApplication)
(define-private-alias Server com.jme3.network.Server)

;;; ---------------------------------------------------------------------
;;; server parameters
;;; ---------------------------------------------------------------------

(define server-port (make-parameter 6143))
(define server-address (make-parameter "localhost"))

;;; ---------------------------------------------------------------------
;;; <fabric-server> - the server class
;;; ---------------------------------------------------------------------

(define-simple-class <fabric-server> (SimpleApplication)
  ;; slots
  ;; -------
  (network-server::Server init-form: #!null)

  ;; accessors
  ;; ---------
  ((getNetworkServer) network-server)
  ((setNetworkServer server::Server) (set! network-server server))

  ;; methods
  ;; -------
  ((simpleInitApp) (begin (set! network-server (Network:createServer (server-port)))
                          (invoke network-server 'start)))
  ((stopServer) (invoke network-server 'close)))


;;; ---------------------------------------------------------------------
;;; accessors
;;; ---------------------------------------------------------------------

(define (get-network-server server)
  (@ 'getNetworkServer server))

(define (set-network-server! server network-server)
  (@ 'setNetworkServer server network-server))

;;; ---------------------------------------------------------------------
;;; initialization
;;; ---------------------------------------------------------------------

(define (make-server)
  (let* ((server (<fabric-server>)))
    server))

;;; ---------------------------------------------------------------------
;;; startup and shutdown
;;; ---------------------------------------------------------------------

(define (start-server server::<fabric-server>)
  (let ((context-type Context:Type))
    (invoke server 'start context-type:Headless)))

(define (stop-server server::<fabric-server>)
  (server:stopServer))



