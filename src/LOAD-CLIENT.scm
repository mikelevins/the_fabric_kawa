;;;; ***********************************************************************
;;;;
;;;; Name:          LOAD-CLIENT.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the game login and loading client
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; load this file in order to load the Fabric client in an
;;; interactive session

(require "version.scm")
(require "util-error.scm")
(require "util-java.scm")
(require "model-id.scm")
(require "model-klos.scm")
(require "init-config-local.scm")
;;(require "init-config.scm")
(require "util-crypt.scm")
(require "model-entity.scm")
(require "model-namegen.scm")
(require "data-assets.scm")
(require "net-messaging.scm")
(require "client-main.scm")
(require "syntax-time.scm")

;;; ---------------------------------------------------------------------
;;; loading
;;; ---------------------------------------------------------------------
;;; evaluate these expressions to create and run the Fabric client

;;; (define $client::FabricClient (make-client))
;;; (*:start $client)
;;; switch to the character creator
;;; (enqueue-mode-update $client 'create-character)
;;; (enqueue-mode-update $client 'login)


