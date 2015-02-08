;;;; ***********************************************************************
;;;;
;;;; Name:          LOAD-CLIENT.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the game login and loading client
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(require "version.scm")
(require "util-error.scm")
(require "util-java.scm")
(require "model-id.scm")
(require "model-klos.scm")
(require "init-config-local.scm")
;;(require "init-config.scm")
(require "util-crypt.scm")
(require "model-entity.scm")
(require "model-user.scm")
(require "storage-users.scm")
(require "appstate-login.scm")
(require "client-brocade.scm")
;(require "client-main.scm")
