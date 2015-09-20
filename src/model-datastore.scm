;;;; ***********************************************************************
;;;;
;;;; Name:          model-datastore.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       persistent storage of game data
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************
(format #t "~%loading model-datastore.cm")

(module-export
 Datastore
 init-datastore
 next-id
 snapshot)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require model-player)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class java.lang String))
(import (class java.util Map))
(import (class org.space4j CommandException LoggerException Space Space4J))
(import (class org.space4j.command CreateMapCmd CreateSequenceCmd IncrementSeqCmd PutCmd RemoveCmd))
(import (class org.space4j.implementation SimpleSpace4J))

;;; ---------------------------------------------------------------------
;;; the datastore
;;; ---------------------------------------------------------------------

(define-simple-class Datastore ()
  (map-name::String init: "FabricObjects")
  (sequence-name::String init: "FabricSequence")
  (space4j::Space4J init: #!null)
  (space::Space init: #!null)
  (users::Map init: #!null)
  ((*init* a-space4j::Space4J)(init-datastore (this) a-space4j)))

(define (init-datastore store::Datastore a-space4j::Space4J)
  (set! store:space4j a-space4j)
  (*:start store:space4j)
  (set! store:space (*:getSpace store:space4j))
  (unless (*:check store:space store:map-name)
    (*:exec store:space4j (CreateMapCmd store:map-name))
    (*:exec store:space4j (CreateSequenceCmd store:sequence-name)))
  (set! store:users (*:get store:space store:map-name)))

(define (snapshot store::Datastore)
  (*:executeSnapshot store:space4j))

(define (next-id store::Datastore)
  (*:exec store:space4j (IncrementSeqCmd store:sequence-name)))

;;; (define $space4j (org.space4j.implementation.SimpleSpace4J "fabric_store"))
;;; (define $store (Datastore $space4j))
;;; (init-datastore $store $space4j)

