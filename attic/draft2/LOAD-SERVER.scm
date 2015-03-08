;;;; ***********************************************************************
;;;;
;;;; Name:          LOAD-SERVER.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the game server
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; load this file in order to load the Fabric server in an
;;; interactive session

(require "version.scm")
(require "util-error.scm")
(require "util-java.scm")
(require "model-id.scm")
(require "model-klos.scm")
(require "init-config-local.scm")
;;(require "init-config.scm")
(require "util-crypt.scm")
(require "server-config.scm")
(require "model-entity.scm")
(require "model-user.scm")
(require "storage-users.scm")
(require "net-messaging.scm")
(require "server-main.scm")

;;; ---------------------------------------------------------------------
;;; loading
;;; ---------------------------------------------------------------------
;;; evaluate these expressions to create, start, and stop the Fabric server

;;; (define $server (make-server))
;;; (start-server $server)
;;; (stop-server $server)
