;;;; ***********************************************************************
;;;;
;;;; Name:          server-main.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       fabric server main program
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export make-server register-auth start-server stop-server)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; this file implements the main Fabric server program, which
;;; maintains and updates the software model of the shared world and
;;; provides access to the world for authenticated players. The server
;;; is a long-running headless network service that runs on a host
;;; provided by the Fabric team.

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "version.scm")
(require "util-java.scm")
(require "util-lists.scm")
(require "syntax-classes.scm")
(require "init-config.scm")
(require "net-messaging.scm")
(require "server-messaging.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as Client com.jme3.network.Client)
(import-as Context com.jme3.system.JmeContext)
(import-as Network com.jme3.network.Network)
(import-as Serializable com.jme3.network.serializing.Serializable)
(import-as Serializer com.jme3.network.serializing.Serializer)
(import-as SimpleApplication com.jme3.app.SimpleApplication)
(import-as Server com.jme3.network.Server)

;;; ---------------------------------------------------------------------
;;; FabricServer - the server class
;;; ---------------------------------------------------------------------

(defclass FabricServer (SimpleApplication)
  (slots:
   (network-listener type: Server init-form: #!null getter: getNetworkListener setter: setNetworkListener)
   (chat-handler type: ServerChatHandler init-form: #!null getter: getChatHandler setter: setChatHandler)
   (auth-table init-form: '() getter: getAuthTable setter: setAuthTable))
  (methods:
   ((simpleInitApp) #!void)
   ((stopServer) (*:close network-listener))
   ((printServer)(begin (format #t "~%The Fabric server: ~A" (fabric-version))
                        (format #t "~% network listener: ~S" network-listener)
                        (format #t "~% listener running? ~S" (and (not (jnull? network-listener))
                                                                  (*:isRunning network-listener)))
                        (format #t "~% ~A" (*:toString network-listener))))))

(define (register-auth server::FabricServer username token)
  (let* ((old-table (*:getAuthTable server))
         (new-table (put-key old-table username token)))
    (*:setAuthTable server new-table)))

;;; ---------------------------------------------------------------------
;;; initialization
;;; ---------------------------------------------------------------------

(define (make-server)
  (Serializer:registerClass ChatMessage)
  (Serializer:registerClass RequestLoginMessage)
  (Serializer:registerClass ResponseLoginMessage )
  (Serializer:registerClass RequestCreateAccountMessage)
  (Serializer:registerClass ResponseCreateAccountMessage)
  (let* ((server (FabricServer)))
    server))

;;; ---------------------------------------------------------------------
;;; startup and shutdown
;;; ---------------------------------------------------------------------

(define (start-server app::FabricServer)
  (let ((listener (Network:createServer (server-name) (server-version) (server-port)(server-port)))
        (chat-handler (ServerChatHandler)))
    (*:setNetworkListener app listener)
    (*:start listener)
    (*:addMessageListener listener chat-handler ChatMessage)
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
