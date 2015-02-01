;;;; ***********************************************************************
;;;;
;;;; Name:          server-main.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       fabric server main program
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export make-server start-server stop-server)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "version.scm")
(require "util-java.scm")
(require "syntax-classes.scm")
(require "init-config.scm")
(require "net-messaging.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as Client com.jme3.network.Client)
(import-as Context com.jme3.system.JmeContext)
(import-as HostedConnection com.jme3.network.HostedConnection)
(import-as MessageListener com.jme3.network.MessageListener)
(import-as Network com.jme3.network.Network)
(import-as Serializable com.jme3.network.serializing.Serializable)
(import-as Serializer com.jme3.network.serializing.Serializer)
(import-as SimpleApplication com.jme3.app.SimpleApplication)
(import-as Server com.jme3.network.Server)

;;; ---------------------------------------------------------------------
;;; ServerChatHandler - aux class for handling icoming chat messages
;;; ---------------------------------------------------------------------

(defclass ServerChatHandler (MessageListener)
  (methods:
   ((messageReceived source::HostedConnection  msg::ChatMessage)
    (format #t "~%Received message: ~s" msg)
    (*:setAttribute source "name" (*:getName msg))
    (format #t "~%Broadcasting message: ~a..." (*:toString msg))
    (*:broadcast (*:getServer source) msg))))

;;; ---------------------------------------------------------------------
;;; FabricServer - the server class
;;; ---------------------------------------------------------------------

(defclass FabricServer (SimpleApplication)
  (slots:
   (network-listener type: Server init-form: #!null getter: getNetworkListener setter: setNetworkListener)
   (chat-handler type: ServerChatHandler init-form: #!null getter: getChatHandler setter: setChatHandler))
  (methods:
   ((simpleInitApp) #!void)
   ((stopServer) (*:close network-listener))
   ((printServer)(begin (format #t "~%The Fabric server: ~A" (fabric-version))
                        (format #t "~% network listener: ~S" network-listener)
                        (format #t "~% listener running? ~S" (and (not (jnull? network-listener))
                                                                  (*:isRunning network-listener)))
                        (format #t "~% ~A" (*:toString network-listener))))))

;;; ---------------------------------------------------------------------
;;; initialization
;;; ---------------------------------------------------------------------

(define (make-server)
  (Serializer:registerClass ChatMessage)
  (let* ((server (FabricServer)))
    server))

;;; ---------------------------------------------------------------------
;;; startup and shutdown
;;; ---------------------------------------------------------------------

(define (start-server app::FabricServer)
  (let ((listener (Network:createServer (server-name) (server-version) (server-port)(server-port)))
        (handler (ServerChatHandler)))
    (*:setNetworkListener app listener)
    (*:start listener)
    (*:addMessageListener listener handler ChatMessage)
    (*:start app Context:Type:Headless)
    (*:printServer app)))

(define (stop-server app::FabricServer)
  (let ((listener::Server (*:getNetworkListener app)))
    (*:setNetworkListener app #!null)
    (*:close listener)
    (*:stop app)))

;;; (define $server (make-server))
;;; (start-server $server)
;;; (stop-server $server)
