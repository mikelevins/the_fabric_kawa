;;;; ***********************************************************************
;;;;
;;;; Name:          listener-message-server.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the server's MessageListener
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 FabricServerMessageListener)

(import (class com.jme3.network AbstractMessage HostedConnection Message MessageListener Server))
(import (class java.lang String))

(define-simple-class FabricServerMessageListener (MessageListener)
  ((messageReceived source::HostedConnection message::Message)
   (format #t "~%Server connection ~S received a message: ~S" source message)))

