;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          server.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       server main program
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export make-server start-listener stop-listener)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "init-config.scm")
(require "net-messaging.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(define-private-alias Context com.jme3.system.JmeContext)
(define-private-alias MessageListener com.jme3.network.MessageListener)
(define-private-alias Network com.jme3.network.Network)
(define-private-alias SimpleApplication com.jme3.app.SimpleApplication)
(define-private-alias Server com.jme3.network.Server)


;;; ---------------------------------------------------------------------
;;; ServerChatHandler - aux class for handling icoming chat messages
;;; ---------------------------------------------------------------------

(define-simple-class ServerChatHandler (MessageListener)
  ((messageReceived source msg) (if (instance? msg ChatMessage)
                                    (begin (*:setAttribute source "name" (*:getName msg))
                                           (*:broadcast (*:getServer source)
                                                        msg))
                                    (format #t "Unrecognized message: ~s" msg))))

;;; ---------------------------------------------------------------------
;;; FabricServer - the server class
;;; ---------------------------------------------------------------------

(define-simple-class FabricServer (SimpleApplication)
  ;; slots
  ;; -------
  (network-listener::Server init-form: #!null)
  (chat-handler::ServerChatHandler init-form: #!null)

  ;; accessors
  ;; ---------
  ((getNetworkListener) network-listener)
  ((setNetworkListener listener::Server) (set! network-listener listener))

  ((getChatHandler) chat-handler)
  ((setChatHandler handler::ServerChatHandler) (set! chat-handler handler))

  ;; methods
  ;; -------
  ((simpleInitApp) #!void)
  ((stopServer) (*:close network-listener)))


;;; ---------------------------------------------------------------------
;;; accessors
;;; ---------------------------------------------------------------------

(define (network-listener app)(*:getNetworkListener app))
(define (set-network-listener! app listener)(*:setNetworkListener app listener))

;;; ---------------------------------------------------------------------
;;; initialization
;;; ---------------------------------------------------------------------

(define (make-server)
  (let* ((server (FabricServer)))
    server))

;;; ---------------------------------------------------------------------
;;; startup and shutdown
;;; ---------------------------------------------------------------------

(define (start-listener app)
  (let ((listener (Network:createServer (server-name) (server-version) (server-port)(server-port)))
        (handler (ServerChatHandler)))
    (set-network-listener! app listener)
    (*:start listener)
    (*:addMessageListener listener handler ChatMessage:class)))

(define (stop-listener app)
  (let ((listener (network-listener app)))
    (set-network-listener! app #!null)
    (*:close listener)))
