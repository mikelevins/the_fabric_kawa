;;;; ***********************************************************************
;;;; Name:          application-common.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       common definitions for Fabric apps
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export FabricApp make-app make-default-scene)

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
(define-private-alias Spatial com.jme3.scene.Spatial)


;;; ---------------------------------------------------------------------
;;; FabricApp initialization
;;; ---------------------------------------------------------------------

;;; helper functions

(define (%get-gui-screen app)
  (let* ((slots app:slots)
         (screen (*:get slots gui-screen:)))
    (when (jnull? screen)
      (set! screen (Screen app))
      (*:initialize screen)
      (*:addControl (get-key app gui-node:) screen)
      (set! app:slots (*:plus app:slots gui-screen: screen)))
    screen))

(define (%ensure-settings app)
  (begin (when (absent? (*:getSettings app))
           (*:setSettings app (AppSettings #t)))
         (*:getSettings app)))

(define (%clear-skybox! app)
  (let* ((root::Node (*:getRootNode app))
         (already (*:getChild root "skybox")))
    (when already
      (unless (absent? already)
        (*:detachChild root already)))))

(define (%set-skybox app val)
  (let* ((root::Node (*:getRootNode app))
         (set-sky! (lambda (sky)(*:attachChild root sky))))
    (if (absent? val)
        (%clear-skybox! app)
        (let ((sky::Spatial val))
          (if (*:equals "skybox" (*:getName sky))
              (begin (%clear-skybox! app)
                     (set-sky! sky))
              (error "The new skybox must be a Node or Spatial whose name is \"skybox\""))))))

;;; set up frame slots, getters, and setters

(define (%default-fabricapp-slots)
  (hashpmap application-init: make-default-scene))

(define (%default-fabricapp-slot-getters)
  (hashpmap
   root-node: (lambda (app key)(*:getRootNode app))
   settings: (lambda (app key)(%ensure-settings app))
   gui-node: (lambda (app key)(*:getGuiNode app))
   gui-screen: (lambda (app key)(%get-gui-screen app))
   viewport: (lambda (app key)(*:getViewPort app))
   camera: (lambda (app key)(*:getCamera app))
   flyby-camera: (lambda (app key)(*:getFlyByCamera app))
   skybox: (lambda (app key)(*:getChild (*:getRootNode app) "skybox"))))

(define (%default-fabricapp-slot-setters)
  (hashpmap
   root-node: (lambda (app key val)(error "root-node: is read-only"))
   gui-node: (lambda (app key val)(error "gui-node: is read-only"))
   gui-screen: (lambda (app key val)(error "gui-screen: is read-only"))
   viewport: (lambda (app key val)(error "viewport: is read-only"))
   flyby-camera: (lambda (app key val)(error "flyby-camera: is read-only"))
   settings: (lambda (app key val)(*:setSettings app val))
   skybox: (lambda (app key val)(%set-skybox app val))))

;;; ---------------------------------------------------------------------
;;; a default scene
;;; ---------------------------------------------------------------------

(define (make-default-scene app)
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
;;; FabricApp - the common client application class
;;; ---------------------------------------------------------------------

(define-simple-class FabricApp (SimpleApplication ISlotAccessors)
  ;; slots
  ;; ---------
  ((getSettings) settings) ; getter for inherited settings field

  (slots::HashPMap init-form: (%default-fabricapp-slots))
  ((getSlots) slots)
  ((setSlots new-slots) (set! slots new-slots))

  (slotGetters::HashPMap init-form: (%default-fabricapp-slot-getters))
  ((getSlotGetter key) (*:get slotGetters key))
  ((setSlotGetter key getter) (set! slotGetters (*:plus slotGetters key getter)))
  ((deleteSlotGetter key) (set! slotGetters (*:minus slotGetters key)))

  (slotSetters::HashPMap init-form: (%default-fabricapp-slot-setters))
  ((getSlotSetter key) (*:get slotSetters key))
  ((setSlotSetter key setter) (set! slotSetters (*:plus slotSetters key setter)))
  ((deleteSlotSetter key) (set! slotSetters (*:minus slotSetters key)))
  
  ;; IFrame, IMutableFrame, ISlotAccessors
  ;; ---------
  ((frameKeys) (append (list root-node: gui-node:)
                       (map-keys slots)))
  ((containsFrameKey key) (member key (*:frameKeys (this))))
  ((getFrameKey key)(let ((getter (*:getSlotGetter (this) key)))
                      (if (absent? getter)
                          (*:get slots key)
                          (getter (this) key))))
  ((setFrameKey key val)(let ((setter (*:getSlotSetter (this) key)))
                          (if (absent? setter)
                              (set! slots (*:plus slots key val))
                              (setter (this) key val))))
  ((deleteFrameKey key) (error "Cannot delete slots from a FabricApp!"))
  
  ;; SimpleApplication
  ;; ---------
  ((simpleInitApp)(let ((init (get-key (this) application-init:)))
                    (init (this)))))

;;; ---------------------------------------------------------------------
;;; make an app
;;; ---------------------------------------------------------------------

(define (make-app)(FabricApp))

;; (define $app (make-app))
;; (*:start $app)

