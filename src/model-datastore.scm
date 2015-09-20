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

(import-as CommandException org.space4j.CommandException)
(import-as CreateMapCmd org.space4j.command.CreateMapCmd)
(import-as CreateSequenceCmd org.space4j.command.CreateSequenceCmd)
(import-as IncrementSeqCmd org.space4j.command.IncrementSeqCmd)
(import-as LoggerException org.space4j.LoggerException)
(import-as Map java.util.Map)
(import-as PutCmd org.space4j.command.PutCmd)
(import-as RemoveCmd org.space4j.command.RemoveCmd)
(import-as SimpleSpace4J org.space4j.implementation.SimpleSpace4J)
(import-as Space org.space4j.Space)
(import-as Space4J org.space4j.Space4J)
(import-as String java.lang.String)

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

