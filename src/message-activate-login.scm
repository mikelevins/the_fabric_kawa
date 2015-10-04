;;;; ***********************************************************************
;;;;
;;;; Name:          message-activate-login.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       a message that commands teh client to activate the login state
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 ActivateLoginMessage)

(require message)

(import (class com.jme3.network AbstractMessage))
(import (class com.jme3.network.serializing Serializable))
(import (class java.lang String))

(define-simple-class ActivateLoginMessage (FabricMessage) (@Serializable)
  (name type: String init: "ActivateLoginMessage")
  ((*init*) #!void))

