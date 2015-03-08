;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          init-config.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       startup parameters, networked configuration
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export fabric-root server-name server-version server-port server-host)

(define (fabric-root) (get-environment-variable "FABRIC_ROOT"))
(define (server-name) "The Fabric Server")
(define (server-version) 1)
(define (server-port) 6143)
(define (server-host) "explorersguild.com")

;;; to run on xg we do:
;;; $ tmux
;;; Ctrl-B $ to name the session
;;; Ctrl-B D to detach the session. We can now log off and the session continues.
;;; To re-attach:
;;; 1. Log in
;;; 2. (optionally) run tmux list-sessions to find the name of the session
;;; 3. tmux attach <session-name>
;;;
;;; the server runs a Kawa repl. You can get and inspect
;;; the running game-server object using the function
;;; (FabricServer:fabric-manager)

