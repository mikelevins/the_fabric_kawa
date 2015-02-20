;;;; ***********************************************************************
;;;;
;;;; Name:          data-assets.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       asset-manager utils
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export get-asset-manager)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; This file provides a simple utility for obtaining a reference to
;;; the JMonkeyEngine 3 desktop asset manager, a singleton object that
;;; loads media assets on demand. The client uses the asset manager
;;; to load textures, icons, and other media for us in the game.

(require "util-java.scm")

;;; ---------------------------------------------------------------------
;;; required imports
;;; ---------------------------------------------------------------------

(import-as JmeSystem com.jme3.system.JmeSystem)
(import-as Thread java.lang.Thread)

;;; ---------------------------------------------------------------------
;;; the asset amanager
;;; ---------------------------------------------------------------------

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define %asset-manager (make-parameter #f))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (new-asset-manager config::java.net.URL)
  (com.jme3.system.JmeSystem:newAssetManager config))

;;; 
;;; ---------------------------------------------------------------------
;;; 

(define (get-asset-manager)
  (or (%asset-manager)
      (begin
        (%asset-manager
         (new-asset-manager
          (get-resource (context-class-loader (current-thread))
                        "com/jme3/asset/Desktop.cfg")))
        (%asset-manager))))





