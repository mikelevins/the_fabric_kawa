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
(define-private-alias Screen tonegod.gui.core.Screen)
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
  
  ;; Frame APIs
  ;; ---------
  ((frameKeys) (append (list root-node: gui-node:)
                       (map-keys slots)))
  ((containsFrameKey key) (member key (*:frameKeys (this))))
  ((getFrameKey key) (cond
                      ((eq? key root-node:) (*:getRootNode (this)))
                      ((eq? key app-settings:) app-settings)
                      ((eq? key gui-node:) (*:getGuiNode (this)))
                      ((eq? key gui-screen:) (let ((screen (*:get slots gui-screen:)))
                                               (when (jnull? screen)
                                                 (set! screen (Screen (this)))
                                                 (*:initialize screen)
                                                 (*:addControl (*:getGuiNode (this)) screen)
                                                 (set! slots (*:plus slots gui-screen: screen)))
                                               screen))
                      ((eq? key viewport:) (*:getViewPort (this)))
                      ((eq? key flyby-camera:) (*:getFlyByCamera (this)))
                      ((eq? key skybox:) (let ((root (*:getRootNode (this))))
                                           (*:getChild root "skybox")))
                      (#t (*:get slots key))))
  ((setFrameKey key val) (cond
                          ((eq? key root-node:) (error "root-node: is read-only"))
                          ((eq? key gui-node:) (error "gui-node: is read-only"))
                          ((eq? key gui-screen:) (error "gui-screen: is read-only"))
                          ((eq? key app-settings:) (error "app-settings: is read-only"))
                          ((eq? key viewport:) (error "viewport: is read-only"))
                          ((eq? key flyby-camera:) (error "flyby-camera: is read-only"))
                          ((eq? key skybox:) (let ((clear-sky! (lambda ()
                                                                 (let* ((root::Node (*:getRootNode (this)))
                                                                        (already (*:getChild root "skybox")))
                                                                   (when already
                                                                     (unless (jnull? already)
                                                                       (*:detachChild root already))))))
                                                   (set-sky! (lambda (sky)
                                                               (let ((root (*:getRootNode (this))))
                                                                 (*:attachChild root sky)))))
                                               (if (or (jnull? val)(not val))
                                                   (clear-sky!)
                                                   (if (*:equals "skybox" (*:getName val))
                                                       (begin (clear-sky!)
                                                              (set-sky! val))
                                                       (error "The new skybox must be a Node or Spatial whose name is \"skybox\"")))))
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

