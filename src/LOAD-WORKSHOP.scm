;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          LOAD-WORKSHOP.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       system loader for the Workshop
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(require 'list-lib)
(require 'srfi-95) ; sorting

(require "util-java.scm")
(require "util-random.scm")
(require "util-general.scm")
(require "util-lists.scm")
(require "init-config.scm")
(require "model-id.scm")
(require "language-types.scm")
(require "language-gf.scm")
(require "model-frame.scm")
(require "model-namegen-domains.scm")
(require "model-namegen.scm")
(require "model-kind.scm")
(require "model-entity.scm")
(require "assets-general.scm")
(require "view-controls.scm")
(require "view-colors.scm")
(require "view-shapes.scm")
(require "view-plasma.scm")
(require "view-name-cube.scm")
(require "view-player.scm")
(require "view-node.scm")
(require "net-messaging.scm")
(require "app-workshop.scm")

;;; (define $workshop (make-workshop))
;;; (*:start $workshop)
