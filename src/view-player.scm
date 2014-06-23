;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          player.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       tools for building player characters
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export make-player-character)

(require "util-java.scm")
(require "assets-general.scm")
(require "model-namegen.scm")
(require "view-name-cube.scm")
(require "model-entity.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(define-private-alias Box com.jme3.scene.shape.Box)
(define-private-alias ColorRGBA com.jme3.math.ColorRGBA)
(define-private-alias Geometry com.jme3.scene.Geometry)
(define-private-alias Material com.jme3.material.Material)
(define-private-alias RenderQueue com.jme3.renderer.queue.RenderQueue)
(define-private-alias RenderState com.jme3.material.RenderState)
(define-private-alias Sphere com.jme3.scene.shape.Sphere)

;;; ---------------------------------------------------------------------
;;; 
;;; ---------------------------------------------------------------------

(define (make-player-character lit-color)
  (let* ((name (gen-name))
         (name-bits (fabric-name-data name))
         (cube (make-name-cube name-bits lit-color)))
    (format #t "~%name-data: ~A" (fabric-name-strings name))
    (format #t "~%name-data: ~d" (fabric-name-data name))
    (format #t "~%name-bytes: ~A~%" (fabric-name-bit-patterns name))
    (entity name: name
            lit-color: lit-color
            name-cube: cube)))

;;; (make-player-character)


