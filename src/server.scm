;;;; ***********************************************************************
;;;;
;;;; Name:          server.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the Fabric server
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require util-error)
(require data-assets)
(require state)
(require state-login)
(require state-create-character)
(require state-pick-character)
(require state-play)
(require state-transition)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import (class com.jme3.app SimpleApplication))
(import (class com.jme3.system AppSettings JmeContext))
(import (class gnu.mapping Symbol))

;;; ---------------------------------------------------------------------
;;; FabricServer
;;; ---------------------------------------------------------------------

(define-variable $server #!null)

(define-simple-class FabricServer (SimpleApplication)
  ;; init
  ((simpleInitApp) (init-server (this))))

(define (init-server app::FabricServer)
  #!void)

(define (make-server #!key
                     (server::FabricServer (FabricServer)))
  server)

(define (start-server)
  (set! $server (make-server))
  (*:start (as FabricServer $server) JmeContext:Type:Headless))

(define (stop-server)
  (*:stop (as FabricServer $server)))
