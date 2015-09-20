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

;;; (define $client::FabricClient (make-client))
;;; (define $fname (generate-fabric-name part-count: (+ 1 (random-integer 4))))
;;; (define $pchar (make-fabric-character $fname))
;;; (*:start $client)
;;; (client-set-login-state! $client)
;;; (client-set-create-character-state! $client $pchar)
;;; (client-set-pick-character-state! $client)
;;; (client-set-play-state! $client $pchar "Pluto")
;;; (client-set-play-state! $client $pchar "Earth")  
;;; (reset-play-state! $client:state)
;;; (client-transit-to! $client $pchar "Jupiter")

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
