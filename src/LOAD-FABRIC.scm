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
(require data-names)
(require data-sexp)
(require model-character)
(require model-namegen)
(require model-user)
(require model-player)
(require state)
(require state-transition)
(require state-login)
(require state-create-character)
(require state-pick-character)
(require state-play)
(require client)

;;; ---------------------------------------------------------------------
;;; loading
;;; ---------------------------------------------------------------------
;;; evaluate these expressions to create and run the Fabric client

;;; (start-client)
;;; (activate-state $client 'login)
;;; (activate-state $client 'create-character)
;;; (activate-state $client 'pick-character)
;;; (activate-state $client 'play)
;;; (activate-state $client 'transition)
;;; (stop-client)


;;; supported transit destinations:
;; Callisto
;; Dione
;; Earth
;; Enceladus
;; Europa
;; Ganymede
;; Iapetus
;; Io
;; Jupiter
;; Mars
;; Mercury
;; Neptune
;; Pluto
;; Rhea
;; Saturn
;; Sedna
;; Tethys
;; The Moon
;; The Sun
;; Titan
;; Uranus
;; Venus
