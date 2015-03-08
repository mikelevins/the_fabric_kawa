;;;; ***********************************************************************
;;;;
;;;; Name:          ui-chatbox.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       network chat UI
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export FabricChat ensure-valid-network-client)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "syntax-classes.scm")
(require "net-messaging.scm")
(require "app-common.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as ChatBox tonegod.gui.controls.extras.ChatBox)
(import-as ConnectException java.net.ConnectException)
(import-as MessageListener com.jme3.network.MessageListener)
(import-as Network com.jme3.network.Network)
(import-as Screen tonegod.gui.core.Screen)
(import-as String java.lang.String)
(import-as Vector2f com.jme3.math.Vector2f)

;;; ---------------------------------------------------------------------
;;; the chatbox
;;; ---------------------------------------------------------------------

;;; aux class for handling incoming chat messages
;;; ---------------------------------------------------------------------

(defclass ClientChatHandler (MessageListener)
  (slots: (application type: FabricApp init-form: #!null))
  (methods:
   ((*init* app)(set! application app))
   ((messageReceived source msg)
    (if (instance? msg ChatMessage)
        (let* ((chatbox (*:getChatHud application))
               (msg-name (*:getName msg))
               (msg-contents (*:getContents msg))
               (received-text (format #f "[~A] ~A" msg-name msg-contents))
               (updater (runnable (lambda ()(*:receiveMsg chatbox received-text)))))
          (*:enqueue application updater))
        (format #t "Unrecognized message: ~s" msg)))))

;;; helper functions
;;; ---------------------------------------------------------------------

(define (report-failed-chat-message app chat-message chat-box)
  (let* ((msg-name (*:getName chat-message))
         (msg-contents (*:getContents chat-message))
         (failed-text (format #f "Connection failed; unable to send message: [~A] ~A"
                              msg-name msg-contents)))
    (*:receiveMsg chat-box failed-text)))

(define (connect-to-server app)
  (try-catch
   (let ((new-connection (Network:connectToServer (server-name)(server-version)(server-host)
                                                  (server-port)(server-port))))
     (*:setNetworkClient app new-connection)
     (*:addMessageListener (*:getNetworkClient app) (ClientChatHandler app))
     (*:start (*:getNetworkClient app)))
   (ex ConnectException (begin (*:setNetworkClient app #!null)
                               (format #t "~%Failed to connect to Fabric server.")
                               (format #t "~%~A" (*:toString ex))))))

(define (ensure-valid-network-client app)
  (let ((net-client #f)
        (found-client (*:getNetworkClient app)))
    (when (jnull? found-client)
      (connect-to-server app))
    (set! net-client (*:getNetworkClient app))
    (if (jnull? net-client)
        #f
        (if (*:isConnected net-client)
            net-client
            #f))))

(define (send-chat-message app chat-message)
  (let ((net-client (ensure-valid-network-client app)))
    (if net-client
        (*:send net-client chat-message)
        (report-failed-chat-message app chat-message (*:getChatHud app)))))

;;; the chatbox class
;;; ---------------------------------------------------------------------

(defclass FabricChat (ChatBox)
  (slots: (chatname type: String init-form: "" getter: getChatName setter: setChatName))
  (methods:
   ((*init* screen :: Screen id :: String position :: Vector2f size :: Vector2f)
    (invoke-special ChatBox (this) '*init* screen id position size))
   ((onSendMsg msg :: String)
    (let* ((chatfield (*:getChildElementById (this) "chatbox:ChatInput"))
           (screen (*:getScreen (this)))
           (app (*:getApplication screen))
           (chat-message (ChatMessage)))
      (*:setName chat-message chatname)
      (*:setContents chat-message msg)
      (*:setReliable chat-message #t)
      (send-chat-message app chat-message)
      (*:resetTabFocus chatfield)))))
