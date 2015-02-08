;;;; ***********************************************************************
;;;;
;;;; Name:          appstate-login.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       log in to the Fabric server
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export LoginAppState)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "syntax-classes.scm")
(require "net-messaging.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as AbstractAppState com.jme3.app.state.AbstractAppState)
(import-as Client com.jme3.network.Client)
(import-as ConnectException java.net.ConnectException)
(import-as DefaultClient com.jme3.network.base.DefaultClient)
(import-as Network com.jme3.network.Network)
(import-as Serializer com.jme3.network.serializing.Serializer)

;;; ---------------------------------------------------------------------
;;; the LoginAppState class
;;; ---------------------------------------------------------------------

(defclass LoginAppState (AbstractAppState)
  (slots:
   (network-client::com.jme3.network.Client
    init-form: #!null getter: getNetworkClient setter: setNetworkClient)))

;;; ---------------------------------------------------------------------
;;; network connectivity
;;; ---------------------------------------------------------------------

(define (connect-to-server state::LoginAppState)
  (try-catch
   (let ((new-connection (Network:connectToServer (server-name)(server-version)(server-host)
                                                  (server-port)(server-port))))
     (*:setNetworkClient state new-connection)
     ;;(*:addMessageListener new-connection (ClientChatHandler state))
     (*:start new-connection))
   (ex ConnectException (begin (*:setNetworkClient state #!null)
                               (warn "failed to connect to Fabric server.")
                               (warn "~A" (*:toString ex))))))

(define (ensure-valid-network-client state::LoginAppState)
  (let ((net-client::Client #!null)
        (found-client::Client (*:getNetworkClient state)))
    (when (jnull? found-client)
      (connect-to-server state))
    (set! net-client (*:getNetworkClient state))
    (if (jnull? net-client)
        net-client
        (if (*:isConnected net-client)
            net-client
            #!null))))