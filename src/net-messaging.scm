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
 RequestCreateAccountMessage
 ServerChatHandler)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "util-lists.scm")
(require "syntax-classes.scm")
(require "model-id.scm")
(require "model-auth.scm")
(require "server-main.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as AbstractMessage com.jme3.network.AbstractMessage)
(import-as Filters com.jme3.network.Filters)
(import-as HostedConnection com.jme3.network.HostedConnection)
(import-as MessageListener com.jme3.network.MessageListener)
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
   (passwordHash type: String init-form: #!null getter: getPasswordHash setter: setPasswordHash))
  (methods:
   ((toString) (format #f "Create: ~A" username))))

;;; ---------------------------------------------------------------------
;;; RequestLoginMessage
;;; ---------------------------------------------------------------------

(defclass RequestLoginMessage (AbstractMessage)
  (annotations: @Serializable)
  (slots:
   (username type: String init-form: #!null getter: getUsername setter: setUsername)
   (passwordHash type: String init-form: #!null getter: getPasswordHash setter: setPasswordHash))
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

;;; =====================================================================
;;; Message Handlers
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; ServerChatHandler - aux server class for handling incoming chat messages
;;; ---------------------------------------------------------------------

(defclass ServerChatHandler (MessageListener)
  (methods:
   ((messageReceived source::HostedConnection  msg::ChatMessage)
    (format #t "~%Received message: ~s" msg)
    (*:setAttribute source "name" (*:getName msg))
    (format #t "~%Broadcasting message: ~a..." (*:toString msg))
    (*:broadcast (*:getServer source) msg))))

;;; ---------------------------------------------------------------------
;;; ServerAuthHandler - aux server class for handling incoming auth messages
;;; ---------------------------------------------------------------------

(defclass ServerAuthHandler (MessageListener)
  (methods:
   ((messageReceived source::HostedConnection  msg::RequestLoginMessage)
    (let* ((username (*:getUsername msg))
           (user-entity (find-username username))
           (offered-password-hash (*:getPasswordHash msg)))
      (format #t "~%Received message: ~s" msg)
      (if user-entity
          (if (equal? offered-password-hash
                      (get-key (entity-properties user-entity)
                               password:
                               #f))
              ;; auth successful; send auth token
              (let* ((auth-token (AuthToken))
                     (auth-message (ResponseLoginMessage)))
                (*:setUsername auth-token username)
                (*:setId auth-token (id->string (makeid)))
                (*:setSucceeded auth-message #t)
                (*:setAuthToken auth-message auth-token)
                (*:setStatus auth-message "succeeded")
                (register-auth (*:getServer source) username auth-token)
                (*:broadcast (*:getServer source) (Filters:in source) auth-message))
              ;; auth failed; send failure message
              (let* ((not-auth-message (ResponseLoginMessage)))
                (*:setSucceeded not-auth-message #f)
                (*:setAuthToken not-auth-message #f)
                (*:setStatus not-auth-message "failed")
                (*:broadcast (*:getServer source) (Filters:in source) not-auth-message)))
          ;; no user found; send failure message
          (let* ((not-auth-message (ResponseLoginMessage)))
            (*:setSucceeded not-auth-message #f)
            (*:setAuthToken not-auth-message #f)
            (*:setStatus not-auth-message "failed")
            (*:broadcast (*:getServer source) (Filters:in source) not-auth-message)))))))
