;;;; ***********************************************************************
;;;; Name:          application-common.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       common definitions for Fabric apps
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export FabricApp app-settings gui-node make-app root-node)

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
(define-private-alias Box com.jme3.scene.shape.Box)
(define-private-alias ColorRGBA com.jme3.math.ColorRGBA)
(define-private-alias Geometry com.jme3.scene.Geometry)
(define-private-alias Material com.jme3.material.Material)
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
  ((deleteKey key) (*:deleteKey frame-state key))
  ;; SimpleApplication
  ((simpleInitApp)(let ((init (or (get-key frame-state application-init: #f)
                                  (lambda (app) (default-init-application app)))))
                    (init (this)))))

;;; ---------------------------------------------------------------------
;;; accessor functions
;;; ---------------------------------------------------------------------

(defgetter (app-settings FabricApp) getAppSettings)

(define (root-node app)(*:getRootNode app))
(define (gui-node app)(*:getGuiNode app))

;;; ---------------------------------------------------------------------
;;; application initialization
;;; ---------------------------------------------------------------------

(define (default-init-application app)
  (let* ((asset-manager (get-asset-manager))
         (box (Box 1 1 1))
         (geom (Geometry "Box" box))
         (mat (Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
         (root (root-node app)))
    (*:setColor mat "Color" ColorRGBA:Blue)
    (*:setMaterial geom mat)
    (*:attachChild root geom)
    app))

(define (make-app)(FabricApp))
