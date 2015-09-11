;;;; ***********************************************************************
;;;;
;;;; Name:          LOAD-FABRIC.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       common definitions for the client and server
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; load this file in order to load the Fabric client in an
;;; interactive session

(require "util-random.scm")
(require "util-bytes.scm")
(require "data-names.scm")
(require "model-namegen.scm")
(require "model-character.scm")
(require "client-class.scm")
(require "state-play.scm")

;;; ---------------------------------------------------------------------
;;; loading
;;; ---------------------------------------------------------------------
;;; evaluate these expressions to create and run the Fabric client

;;; (define $client::FabricClient (make-client))
;;; (define $pchar (make-fabric-character))
;;; (*:start $client)
;;; (client-set-login-state! $client)
;;; (client-set-create-character-state! $client $pchar)
;;; (client-set-pick-character-state! $client)
;;; (client-set-play-state! $client $pchar "Earth")
;;; (reset-play-state! $client:state)
;;; (client-set-transit-state! $client from: "The Sun" to: "Earth")
