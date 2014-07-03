;;;; ***********************************************************************
;;;; Name:          application-common.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       common definitions for Fabric apps
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export FabricApp make-app default-init-application)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "utilities-lists.scm")
(require "utilities-java.scm")
(require "interface-frame.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(define-private-alias AppSettings com.jme3.system.AppSettings)
(define-private-alias Box com.jme3.scene.shape.Box)
(define-private-alias ColorRGBA com.jme3.math.ColorRGBA)
(define-private-alias Geometry com.jme3.scene.Geometry)
(define-private-alias HashPMap org.pcollections.HashPMap)
(define-private-alias HashTreePMap org.pcollections.HashTreePMap)
(define-private-alias LList gnu.lists.LList)
(define-private-alias Map java.util.Map)
(define-private-alias Material com.jme3.material.Material)
(define-private-alias Node com.jme3.scene.Node)
(define-private-alias SimpleApplication com.jme3.app.SimpleApplication)

;;; ---------------------------------------------------------------------
;;; application initialization
;;; ---------------------------------------------------------------------

(define (default-init-application app)
  (let* ((asset-manager (get-asset-manager))
         (box (Box 1 1 1))
         (geom (Geometry "Box" box))
         (mat (Material asset-manager "Common/MatDefs/Misc/Unshaded.j3md"))
         (root::Node (get-key app root-node:)))
    (*:setColor mat "Color" ColorRGBA:Blue)
    (*:setMaterial geom mat)
    (*:attachChild root geom)
    app))

;;; ---------------------------------------------------------------------
;;; FabricApp - the abstract client application class
;;; ---------------------------------------------------------------------

(define-simple-class FabricApp (SimpleApplication IMutableFrame)
  ;; slots
  ;; -------
  (app-settings init-form: (AppSettings #t))
  (slots::HashPMap init-form: (hashpmap application-init: default-init-application))
  
  ;; accessors
  ;; ---------
  ((getAppSettings) app-settings)
  ((getSlots) slots)

  ;; Frame APIs
  ;; ---------
  ((frameKeys) (append (list root-node: gui-node:)
                       (map-keys slots)))
  ((containsFrameKey key) (member key (*:frameKeys (this))))
  ((getFrameKey key) (cond
                      ((eq? key root-node:) (*:getRootNode (this)))
                      ((eq? key gui-node:) (*:getGuiNode (this)))
                      (#t (*:get slots key))))
  ((setFrameKey key val) (cond
                          ((eq? key root-node:) (error "root-node: is read-only"))
                          ((eq? key gui-node:) (error "gui-node: is read-only"))
                          (#t (set! slots (*:plus slots key val)))))
  ((deleteFrameKey key) (error "Cannot delete slots from a FabricApp!"))

  ;; implementation methods
  ;; ---------
  
  ;; SimpleApplication
  ((simpleInitApp)(let ((init (get-key (this) application-init:)))
                    (init (this)))))

;;; ---------------------------------------------------------------------
;;;  FabricApp getters and setters
;;; ---------------------------------------------------------------------


;;; ---------------------------------------------------------------------
;;; make an app
;;; ---------------------------------------------------------------------

(define (make-app)(FabricApp))

;; (define $app (make-app))
;; (*:start $app)

