;;;; ***********************************************************************
;;;;
;;;; Name:          model-character.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       representation of player and non-player characters
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 make-player-character
 PlayerCharacter)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "util-error.scm")
(require "syntax-classes.scm")
(require "view-player-character.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as Node com.jme3.scene.Node)

;;; ---------------------------------------------------------------------
;;; the player character class
;;; ---------------------------------------------------------------------

;;; CLASS PlayerCharacter
;;; ---------------------------------------------------------------------

(defclass PlayerCharacter (Object)
  (slots:
   (node init-form: (Node "PlayerCharacterNode") getter: getNode setter: setNode)
   (name init-form: #f getter: getName setter: setName)
   (faction init-form: #f getter: getFaction setter: setFaction)
   (weapon init-form #f getter: getWeapon setter: setWeapon)
   (armor init-form #f getter: getArmor setter: setArmor)
   (augment init-form #f getter: getAugment setter: setAugment))
  (methods:))

;;; (make-player-character)
;;; ---------------------------------------------------------------------
;;; returns a newly-constructed player-character entity that contains
;;; a cube node suitable for use as its avatar

(define (make-player-character)
  (let* ((character (PlayerCharacter))
         (node (*:getNode character))
         (cube (make-player-character-cube)))
    (*:attachChild node cube)
    character))
