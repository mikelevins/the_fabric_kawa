;;;; ***********************************************************************
;;;;
;;;; Name:          message.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       messages passed between client and server
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 FabricMessage)

(import (class com.jme3.network AbstractMessage))
(import (class com.jme3.network.serializing Serializable))
(import (class java.lang String))

(define-simple-class FabricMessage (AbstractMessage) (@Serializable)
  (name type: String init-form: #!null)
  (contents type: String init-form: #!null)
  ((toString) (format #f "[~A] ~A" name contents)))
