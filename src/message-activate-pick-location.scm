;;;; ***********************************************************************
;;;;
;;;; Name:          message-activate-pick-location.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       a message that commands the client to activate the
;;;;                location-picker state
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 ActivatePickLocationMessage)

(require message)
(require model-user)

(import (class com.jme3.network AbstractMessage))
(import (class com.jme3.network.serializing Serializable))
(import (class java.lang String))

(define-simple-class ActivatePickLocationMessage (FabricMessage) (@Serializable)
  (name type: String init: "ActivatePickLocationMessage")
  (location type: String init: #!null))

