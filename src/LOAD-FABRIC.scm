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
(require client)

;;; ---------------------------------------------------------------------
;;; loading
;;; ---------------------------------------------------------------------
;;; evaluate these expressions to create and run the Fabric client

;;; (start-client)
;;; (activate-state $client 'login)
;;; (activate-state $client 'create-character)
;;; (activate-state $client 'pick-character)
;;; (activate-state $client 'play location: "Jupiter")
;;; (activate-state $client 'transition)
;;; (stop-client)

