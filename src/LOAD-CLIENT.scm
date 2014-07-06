;;;; ***********************************************************************
;;;; Name:          LOAD-CLIENT.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       system loader for the game client
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(require "parameters-version.scm")
(require "setting-hubs.scm")
(require "setting-sky.scm")
(require "setting-celestial-objects.scm")
(require "utilities-math.scm")
(require "utilities-java.scm")
(require "data-pmaps.scm")
(require "interface-frame.scm")
(require "assets-general.scm")
(require "player.scm")
(require "application-common.scm")
(require "application-client.scm")

;;; (define $client (make-client))
;;; (*:start $client)
