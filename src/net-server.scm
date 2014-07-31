;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          net-server.scm
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

(require "version.scm")
(require "util-java.scm")
(require "syntax-classes.scm")
(require "init-config.scm")
(require "net-messaging.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as Context com.jme3.system.JmeContext)
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
   ((messageReceived source msg)
    (format #t "~%Received message: ~s" msg)
    (if (instance? msg ChatMessage)
        (begin (*:setAttribute source "name" (*:getName msg))
               (format #t "~%Broadcasting message: ~a..." (*:toString msg))
               (*:broadcast (*:getServer source) msg))
        (format #t "~%Unrecognized message: ~a~%" (*:toString msg))))))

;;; ---------------------------------------------------------------------
;;; FabricManager - the server class
;;; ---------------------------------------------------------------------

(defclass FabricManager (SimpleApplication)
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
  (let* ((server (FabricManager)))
    server))

;;; ---------------------------------------------------------------------
;;; startup and shutdown
;;; ---------------------------------------------------------------------

(define (start-listener app)
  (let ((listener (Network:createServer (server-name) (server-version) (server-port)(server-port)))
        (handler (ServerChatHandler)))
    (*:setNetworkListener app listener)
    (*:start listener)
    (*:addMessageListener listener handler ChatMessage)
    (*:printServer app)))

(define (stop-listener app)
  (let ((listener (*:getNetworkListener app)))
    (*:setNetworkListener app #!null)
    (*:close listener)))
