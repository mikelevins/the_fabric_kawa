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
(require state-create-character)
(require state-pick-character)
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
(require client)

;;; ---------------------------------------------------------------------
;;; loading
;;; ---------------------------------------------------------------------
;;; evaluate these expressions to create and run the Fabric client

;;; (start-client)
;;; (activate-login-state (the-client))
;;; (activate-create-character-state (the-client)(default-user))
;;; (activate-pick-character-state (the-client)(default-user))
;;; (activate-pick-location-state (the-client)(default-user)(default-character (default-user)))
;;; (activate-play-state (the-client)(default-user)(default-character (default-user)) "Neptune")
;;; (activate-transition-state (the-client))
;;; (stop-client)

