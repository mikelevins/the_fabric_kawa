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
 PlayerCharacter)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "util-error.scm")
(require "syntax-classes.scm")

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
   (weapon init-form #f getter: getWeapon setter: setWeapon)
   (armor init-form #f getter: getArmor setter: setArmor)
   (augment init-form #f getter: getAugment setter: setAugment)
   (alignment init-form #f getter: getAlignment setter: setAlignment))
  (methods:))
