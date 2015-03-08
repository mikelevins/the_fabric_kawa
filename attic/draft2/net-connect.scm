;;;; ***********************************************************************
;;;;
;;;; Name:          net-connect.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       establishing client connections
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 connect-to-server)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as ConnectException java.net.ConnectException)
(import-as Network com.jme3.network.Network)

;;; (connect-to-server)
;;; ---------------------------------------------------------------------
;;; attempts to open a network connection from the Fabric client to
;;; the server in order to pass messages

(define (connect-to-server)
  (try-catch
   (let ((new-connection (Network:connectToServer (server-name)(server-version)(server-host)
                                                  (server-port)(server-port))))
     (*:start new-connection)
     new-connection)
   (ex ConnectException (begin (warn "failed to connect to Fabric server.")
                               (warn "~A" (*:toString ex))
                               #!null))))
