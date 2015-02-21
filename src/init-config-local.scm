;;;; ***********************************************************************
;;;;
;;;; Name:          config.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       startup parameters, local configuration
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 fabric-root
 server-host
 server-name
 server-port
 server-version)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; This is the local version of the Fabric configuration file. It is
;;; loaded at client and server startup in the case where the client
;;; and the server run on the same machine (in other words, during
;;; development).


;;; (fabric-root)
;;; ---------------------------------------------------------------------
;;; the root path of the fabric installation. In development, it's the
;;; root directory of the project.

(define (fabric-root) (get-environment-variable "FABRIC_ROOT"))


;;; (server-name)
;;; ---------------------------------------------------------------------
;;; the name of the Fabric server

(define (server-name) "The Fabric Server")


;;; (server-version)
;;; ---------------------------------------------------------------------
;;; the version of the Fabric server. This number is distinct from the
;;; version number of the Fabric project as a whole, which is a
;;; human-readable version number in traditional "semantic versioning"
;;; style. the server version is a monotonically-increasing integer
;;; that increases each time the server API or protocol
;;; changes. Fabric clients and servers can easily compare server
;;; version numbers in order to determine whether a given client is
;;; compatible with the running server.

(define (server-version) 2)


;;; (server-port)
;;; ---------------------------------------------------------------------
;;; the TCP/IP port on which the Fabric server listens for incoming
;;; connections

(define (server-port) 6143)


;;; (server-host)
;;; ---------------------------------------------------------------------
;;; the IP address or domain name of the Fabric server. In the case of
;;; the local configuration, it's "localhost"

(define (server-host) "localhost")


