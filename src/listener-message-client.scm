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

(require client)

(import (class com.jme3.network AbstractMessage Client Message MessageListener))
(import (class java.lang String))

(define-simple-class FabricClientMessageListener (MessageListener)
  (client::FabricClient init: #!null)
  ((*init* a-client::FabricClient)(set! client a-client))
  ((messageReceived source::Client message::Message)
   (client-handle-message client source message)))

