;;; some net-related functions currently unused and without a home

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
