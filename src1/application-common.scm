;;;; ***********************************************************************
;;;; Name:          application-common.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       common definitions for Fabric apps
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export FabricApp app-settings)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "utilities-java.scm")
(require "interfaces-frame.scm")
(require "model-frame.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(define-private-alias AppSettings com.jme3.system.AppSettings)
(define-private-alias SimpleApplication com.jme3.app.SimpleApplication)

;;; ---------------------------------------------------------------------
;;; FabricApp - the abstract client application class
;;; ---------------------------------------------------------------------

(define-simple-class FabricApp (SimpleApplication IMutableFrame)
  ;; slots
  ;; -------
  (app-settings init-form: (AppSettings #t))
  (frame-state init-form: (AListFrame))

  ;; accessors
  ;; ---------
  ((getAppSettings) app-settings)

  ;; implementation methods
  ;; ---------
  
  ;; IFrame
  ((getKey key) (*:getKey frame-state key))
  ((putKey key val) (*:putKey frame-state key val))
  ((removeKey key) (*:removeKey frame-state key))
  ((containsKey key) (*:containsKey frame-state key))
  ((listKeys) (*:listKeys frame-state))
  ((isEmpty) (*:isEmpty frame-state))
  ;; IMutableFrame
  ((setKey key val) (*:setKey frame-state key val))
  ((deleteKey key) (*:deleteKey frame-state key val))
  ;; SimpleApplication
  ((simpleInitApp)(init-application (this))))

;;; ---------------------------------------------------------------------
;;; accessor functions
;;; ---------------------------------------------------------------------

(defgetter (app-settings FabricApp) getAppSettings)

;;; ---------------------------------------------------------------------
;;; application initialization
;;; ---------------------------------------------------------------------

