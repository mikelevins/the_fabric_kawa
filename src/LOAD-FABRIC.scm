;;;; ***********************************************************************
;;;;
;;;; Name:          LOAD-FABRIC.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       load the Fabric client
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; load this file in order to load the Fabric client in an
;;; interactive session

(require 'list-lib)
(require version)
(require util-random)
(require util-bytes)
(require util-lists)
(require util-crypt)
(require data-file)
(require data-config)
(require data-names)
(require data-sexp)
(require data-users)
(require model-character)
(require model-namegen)
(require model-user)
(require model-rect)
(require state)
(require state-transition)
(require state-login)
(require class-CreateCharacterState)
(require state-create-character)
(require state-pick-character)
(require state-pick-location)
(require state-play)
(require view-alert)
(require view-faction-picker)
(require listener-message-client)
(require listener-message-server)
(require message)
(require message-activate-login)
(require message-activate-create-character)
(require message-activate-pick-character)
(require message-activate-pick-location)
(require message-activate-play)
(require view-client-error)
(require client)

;;; ---------------------------------------------------------------------
;;; loading
;;; ---------------------------------------------------------------------
;;; evaluate these expressions to create and run the Fabric client

;;; (start-client "Jupiter" show-settings: #f record-video: #t)
;;; (activate-login-state)
;;; (activate-create-character-state)
;;; (activate-pick-character-state)
;;; (activate-play-state)
;;; (teleport "Sedna")
;;; (stop-client)

;;; for getting around quickly
;;; (set! (the-client):state:speed 6000)

;;; for looking at small features
;;; (set! (the-client):state:speed 100)aa
