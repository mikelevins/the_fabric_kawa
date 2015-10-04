;;;; ***********************************************************************
;;;;
;;;; Name:          message-activate-play.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       a message that commands teh client to activate the play state
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 ActivatePlayMessage)

(require message)
(require model-user)
(require model-character)

(import (class com.jme3.network AbstractMessage))
(import (class com.jme3.network.serializing Serializable))
(import (class java.lang String))

(define-simple-class ActivatePlayMessage (FabricMessage) (@Serializable)
  (name type: String init: "ActivatePlayMessage")
  (user type: FabricUser init: #!null)
  (character type: FabricCharacter init: #!null)
  (location type: String init: #!null)
  ((*init*) #!void))

