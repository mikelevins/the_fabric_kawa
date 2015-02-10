;;;; ***********************************************************************
;;;;
;;;; Name:          net-messaging.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       fabric network message handling
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export CreateAccountMessage ChatMessage LoginMessage)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "syntax-classes.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as AbstractMessage com.jme3.network.AbstractMessage)
(import-as Serializable com.jme3.network.serializing.Serializable)
(import-as String java.lang.String)

;;; ---------------------------------------------------------------------
;;; CreateAccountMessage
;;; ---------------------------------------------------------------------

(defclass CreateAccountMessage (AbstractMessage)
  (annotations: @Serializable)
  (slots:
   (username type: String init-form: #!null getter: getUserame setter: setUserame)
   (passwordHash type: String init-form: #!null getter: getPasswordHash setter: setPasswordHash))
  (methods:
   ((toString) (format #f "Create: ~A" username))))

;;; ---------------------------------------------------------------------
;;; LoginMessage
;;; ---------------------------------------------------------------------

(defclass LoginMessage (AbstractMessage)
  (annotations: @Serializable)
  (slots:
   (username type: String init-form: #!null getter: getUserame setter: setUserame)
   (passwordHash type: String init-form: #!null getter: getPasswordHash setter: setPasswordHash))
  (methods:
   ((toString) (format #f "Login: ~A" username))))

;;; ---------------------------------------------------------------------
;;; ChatMessage
;;; ---------------------------------------------------------------------

(defclass ChatMessage (AbstractMessage)
  (annotations: @Serializable)
  (slots:
   (name type: String init-form: #!null getter: getName setter: setName)
   (contents type: String init-form: #!null getter: getContents setter: setContents))
  (methods:
   ((toString) (format #f "[~A] ~A" name contents))))

