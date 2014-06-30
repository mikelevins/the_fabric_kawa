;;;; ***********************************************************************
;;;; Name:          application-common.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       common definitions for Fabric apps
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export FabricApp make-app appstate)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "utilities-java.scm")
(require "model-frames.scm")
(require "language-types.scm")
(require "language-gf.scm")

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

(define-simple-class FabricApp (SimpleApplication)
  ;; slots
  ;; -------
  (app-settings init-form: (AppSettings #t))
  (frame-state init-form: (Frame))

  ;; accessors
  ;; ---------
  ((getAppSettings) app-settings)
  ((getFrameState) frame-state)
  ((setFrameState state) (set! frame-state state))

  ;; implementation methods
  ;; ---------
  
  ;; SimpleApplication
  ((simpleInitApp)(let ((init (appstate (this) application-init:)))
                    (init (this)))))

;;; ---------------------------------------------------------------------
;;; application initialization
;;; ---------------------------------------------------------------------

(define (default-init-application app)
  (let* ((asset-manager (get-asset-manager))
         (box (Box 1 1 1))
         (geom (Geometry "Box" box))
         (mat (Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
         (root (appstate app root-node:))
         )
    (*:setColor mat "Color" ColorRGBA:Blue)
    (*:setMaterial geom mat)
    (*:attachChild root geom)
    app))

;;; ---------------------------------------------------------------------
;;; accessor functions
;;; ---------------------------------------------------------------------

(defgeneric appstate)
(defgeneric set-appstate!)

(defmethod appstate ((app FabricApp) (key (singleton app-settings:)))
  (*:getAppSettings app))

(defmethod appstate ((app FabricApp) (key (singleton root-node:)))
  (*:getRootNode app))

(defmethod appstate ((app FabricApp) (key (singleton application-init:)))
  (get-key (*:getFrameState app)
           application-init: default-init-application))

(defmethod set-appstate! ((app FabricApp) (key (singleton application-init:)) (initfn gnu.expr.ModuleMethod))
  (set-key! (*:getFrameState app)
            application-init: initfn))

;;; ---------------------------------------------------------------------
;;; make an app
;;; ---------------------------------------------------------------------

(define (make-app)(FabricApp))


