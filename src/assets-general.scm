;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          assets.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       asset-manager utils
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export get-asset-manager)

(require "util-java.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; assets
;;; ---------------------------------------------------------------------

(define-private-alias JmeSystem com.jme3.system.JmeSystem)
(define-private-alias Thread java.lang.Thread)

(define %asset-manager (make-parameter #f))

(define (get-asset-manager)
  (or (%asset-manager)
      (begin
        (%asset-manager (JmeSystem:newAssetManager
                         (*:getResource (*:getContextClassLoader (Thread:currentThread))
                                         "com/jme3/asset/Desktop.cfg")))
        (%asset-manager))))




