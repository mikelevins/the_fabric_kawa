;;;; ***********************************************************************
;;;;
;;;; Name:          listener-message-client.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the client's MessageListener
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 FabricClientMessageListener)

(import (class com.jme3.network AbstractMessage Client Message MessageListener))
(import (class java.lang String))

(define-simple-class FabricClientMessageListener (MessageListener)
  ((messageReceived source::Client message::Message)
   (format #t "~%Client ~S received a message: ~S" source message)))

