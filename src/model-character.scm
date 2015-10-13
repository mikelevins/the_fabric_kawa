;;;; ***********************************************************************
;;;;
;;;; Name:          model-character.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       The common character class
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 FabricCharacter
 fabric-character-namestring
 make-fabric-character
 make-random-character
 set-fabric-name!)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require util-lists)
(require model-namegen)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class com.jme3.network.serializing Serializable))
(import (class com.jme3.material Material))
(import (class com.jme3.math ColorRGBA))
(import (class com.jme3.scene Geometry Node Spatial))
(import (class com.jme3.util SafeArrayList))
(import (class gnu.mapping Symbol))

;;; ---------------------------------------------------------------------
;;; FabricCharacter
;;; ---------------------------------------------------------------------

(define-simple-class FabricCharacter ()(@Serializable)
  (name::FabricName init: #!null)
  (faction init-form: #!null)
  (weapon init-form: #!null)
  (armor init-form: #!null)
  (augment init-form: #!null))

(define (make-fabric-character fname::FabricName)
  (let* ((fchar (FabricCharacter)))
    (set! fchar:name fname)
    fchar))

(define (fabric-character-namestring fchar::FabricCharacter)
  (apply string-append (interpose " " (fabric-name->strings fchar:name))))

(define (set-fabric-name! fchar::FabricCharacter fname::FabricName)
  (set! fchar:name fname))

;;; a random character used for testing purposes
(define (make-random-character)
  (let* ((fname::FabricName (generate-fabric-name))
         (character (make-fabric-character fname)))
    (set! character:faction (choose-any '(caretakers abjurers rogues)))
    (set! character:armor (choose-any '(absorb regenerate power energy)))
    (set! character:weapon (choose-any '(cannon impulse malware bots)))
    ;;(set! character:augment (choose-any '(force optics portals turrets)))
    (set! character:augment 'turrets)
    character))
