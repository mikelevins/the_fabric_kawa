;;;; ***********************************************************************
;;;;
;;;; Name:          net-messaging.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       fabric network message handling
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 ChatMessage
 RequestLoginMessage
 ResponseLoginMessage
 RequestCreateAccountMessage
 ResponseCreateAccountMessage)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "util-lists.scm")
(require "syntax-classes.scm")
(require "model-id.scm")
(require "model-auth.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as AbstractMessage com.jme3.network.AbstractMessage)
(import-as Filters com.jme3.network.Filters)
(import-as Network com.jme3.network.Network)
(import-as Serializable com.jme3.network.serializing.Serializable)
(import-as String java.lang.String)

;;; =====================================================================
;;; Message types
;;; =====================================================================
;;; all messages except RequestCreateAccountMessage and
;;; RequestLoginMessage require auth tokens in order to be
;;; accepted by client or server. RequestLoginMessage requests
;;; an auth token which is supplied by a successful ResponseLoginMessage.

;;; ---------------------------------------------------------------------
;;; RequestCreateAccountMessage
;;; ---------------------------------------------------------------------

(defclass RequestCreateAccountMessage (AbstractMessage)
  (annotations: @Serializable)
  (slots:
   (username type: String init-form: #!null getter: getUsername setter: setUsername)
   (password type: String init-form: #!null getter: getPassword setter: setPassword))
  (methods:
   ((toString) (format #f "Create: ~A" username))))

;;; ---------------------------------------------------------------------
;;; ResponseCreateAccountMessage
;;; ---------------------------------------------------------------------

(defclass ResponseCreateAccountMessage (AbstractMessage)
  (annotations: @Serializable)
  (slots:
   (username type: String init-form: #!null getter: getUsername setter: setUsername)
   (auth-token init-form: #!null getter: getAuthToken setter: setAuthToken)
   (succeeded? init-form: #!null getter: getSucceeded setter: setSucceeded))
  (methods:
   ((toString) (format #f "Create: ~A" username))))

;;; ---------------------------------------------------------------------
;;; RequestLoginMessage
;;; ---------------------------------------------------------------------

(defclass RequestLoginMessage (AbstractMessage)
  (annotations: @Serializable)
  (slots:
   (username type: String init-form: #!null getter: getUsername setter: setUsername)
   (password type: String init-form: #!null getter: getPassword setter: setPassword))
  (methods:
   ((toString) (format #f "Login: ~A" username))))

;;; ---------------------------------------------------------------------
;;; ResponseLoginMessage
;;; ---------------------------------------------------------------------

(defclass ResponseLoginMessage (AbstractMessage)
  (annotations: @Serializable)
  (slots:
   (succeeded? init-form: #!null getter: getSucceeded setter: setSucceeded)
   (auth-token init-form: #!null getter: getAuthToken setter: setAuthToken)
   (status init-form: #!null getter: getStatus setter: setStatus))
  (methods:
   ((toString) (format #f "Login response: succeeded? ~A" succeeded?))))

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

