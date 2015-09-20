;;;; ***********************************************************************
;;;;
;;;; Name:          model-player.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       modeling players and their accounts
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 FabricPlayer
 )

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require util-error)
(require util-java)
(require util-lists)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class java.lang String))

;;; ---------------------------------------------------------------------
;;; FabricCharacter
;;; ---------------------------------------------------------------------

(define-simple-class FabricPlayer ()
  (username::String init: #!null)
  (characters init: '()))


