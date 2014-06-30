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

(require "parameters-version.scm")
(require "interfaces-frame.scm")
(require "assets-general.scm")
(require "application-common.scm")

;;; (define $app (make-app))
;;; (*:start $app)
