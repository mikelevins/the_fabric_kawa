;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          LOAD-SERVER.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       system loader
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(require "version.scm")
(require "util-java.scm")
(require "util-random.scm")
(require "util-general.scm")
(require "init-config-local.scm")
(require "model-id.scm")
(require "model-frame.scm")
(require "model-namegen-domains.scm")
(require "model-namegen.scm")
(require "model-kind.scm")
(require "model-entity.scm")
(require "net-messaging.scm")
(require "net-server.scm")

;;; (define $the-server (make-server))
;;; (start-listener $the-server)
;;; (stop-listener $the-server)



