;;;; ***********************************************************************
;;;;
;;;; Name:          message-activate-pick-character.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       a message that commands the client to activate the
;;;;                character-picker state
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 ActivatePickCharacterMessage)

(require message)
(require model-user)

(import (class com.jme3.network AbstractMessage))
(import (class com.jme3.network.serializing Serializable))
(import (class java.lang String))

(define-simple-class ActivatePickCharacterMessage (FabricMessage) (@Serializable)
  (name type: String init: "ActivatePickCharacterMessage")
  (user type: FabricUser init: #!null))

