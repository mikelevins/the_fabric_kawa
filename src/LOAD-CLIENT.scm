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

(require "client-main.scm")

;;; ---------------------------------------------------------------------
;;; loading
;;; ---------------------------------------------------------------------
;;; evaluate these expressions to create and run the Fabric client

;;; (define $client::FabricClient (make-client))
;;; (*:start $client)

;;; (set-login-state! $client)
;;; (set-create-character-state! $client)
;;; (set-pick-character-state! $client)
;;; (set-play-state! $client node: "Tethys")
;;; (set-transit-state! $client)
;;; (set-workshop-state! $client)
