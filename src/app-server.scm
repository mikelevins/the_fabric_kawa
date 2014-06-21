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
;;; server parameters
;;; ---------------------------------------------------------------------

(define server-port (make-parameter 6143))
(define server-address (make-parameter "localhost"))

;;; ---------------------------------------------------------------------
;;; the server class
;;; ---------------------------------------------------------------------

(define-simple-class <fabric-server> (com.jme3.app.SimpleApplication)
  ;; slots
  (network-server::com.jme3.network.Server init-form: #!null)
  ;; methods
  ((simpleInitApp) (begin (set! network-server (com.jme3.network.Network:createServer (server-port)))
                          (invoke network-server 'start)))
  ((stopServer) (invoke network-server 'close)))

(define (make-server)
  (let* ((server (<fabric-server>)))
    server))

(define (start-server server::<fabric-server>)
  (let ((context-type com.jme3.system.JmeContext:Type))
    (invoke server 'start context-type:Headless)))

(define (stop-server server::<fabric-server>)
  (server:stopServer))



