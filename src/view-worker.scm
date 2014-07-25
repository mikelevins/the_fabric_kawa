;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          view-worker.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       an avatar for use in the workshop
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export make-worker-character make-armors worker-namestring)

(require 'list-lib)
(require "util-java.scm")
(require "util-lists.scm")
(require "assets-general.scm")
(require "model-namegen.scm")
(require "view-name-cube.scm")
(require "model-entity.scm")
(require "model-frame.scm")
(require "view-shapes.scm")
(require "view-controls.scm")
(require "view-plasma.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as Box com.jme3.scene.shape.Box)
(import-as ColorRGBA com.jme3.math.ColorRGBA)
(import-as Geometry com.jme3.scene.Geometry)
(import-as Material com.jme3.material.Material)
(import-as RenderQueue com.jme3.renderer.queue.RenderQueue)
(import-as RenderState com.jme3.material.RenderState)
(import-as Sphere com.jme3.scene.shape.Sphere)

;;; ---------------------------------------------------------------------
;;; character armor
;;; ---------------------------------------------------------------------

;;; $available-armorers
;;; ---------------------------------------------------------------------
;;; armor constructors

(define $available-armorers
  (list make-enclosing-cube
        make-enclosing-wire-cube
        make-enclosing-pyramid
        make-enclosing-sphere
        make-enclosing-wire-sphere))

;;; (make-armors n)
;;; ---------------------------------------------------------------------
;;; make N randomly-chosen armors

(define (make-armors n)
  (let ((armorers (cons make-any-plasma-generator
                        (map (lambda (a)(choose-any $available-armorers))
                             (iota n)))))
    (map (lambda (make-armor)
           (let ((armor::Geometry (make-armor))
                 (rotator (any-rotator)))
             (*:addControl armor rotator)
             armor))
         armorers)))


;;; ---------------------------------------------------------------------
;;; construct a character
;;; ---------------------------------------------------------------------

(define (make-worker-character lit-color)
  (let* ((name (java.lang.System:getProperty "user.name"))
         (shape (make-worker-sphere)))
    (entity name: name
            lit-color: lit-color
            shape: shape)))

(define (worker-namestring worker)
  (let* ((namestring (get-key worker name: "worker")))
    (*:toString  namestring)))



