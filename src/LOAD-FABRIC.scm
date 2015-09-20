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
(require model-namegen)
(require model-character)
(require model-player)
(require client-class)
(require state-play)

;;; ---------------------------------------------------------------------
;;; loading
;;; ---------------------------------------------------------------------
;;; evaluate these expressions to create and run the Fabric client

;;; (start-client)
;;; (client-set-login-state! $client)
;;; (client-set-create-character-state! $client $character)
;;; (client-set-pick-character-state! $client)
;;; (client-set-play-state! $client $character "Pluto")
;;; (reset-play-state! $client:state)
;;; (client-transit-to! $client $character "Jupiter")

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
