;;;; ***********************************************************************
;;;;
;;;; Name:          model-character.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       The common character class
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 FabricCharacter
 make-fabric-character
 set-fabric-name!)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-error.scm")
(require "util-java.scm")
(require "util-lists.scm")
(require "model-namegen.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as Node com.jme3.scene.Node)

;;; ---------------------------------------------------------------------
;;; FabricCharacter
;;; ---------------------------------------------------------------------

(define-simple-class FabricCharacter ()
  (name init: #!null type: FabricName)
  (node init: #!null type: Node))

(define (make-fabric-character #!optional fabric-name)
  (let* ((fname::FabricName (or fabric-name (random-fabric-name)))
         (fname-strings (fabric-name-strings fname))
         (fname-string (apply string-append (interpose " " fname-strings)))
         (fnode (Node fname-string))
         (fchar (FabricCharacter)))
    (set! fchar:node fnode)
    (set! fchar:name fname)
    fchar))

(define (set-fabric-name! fchar::FabricCharacter fname::FabricName)
  (let* ((fname-strings (fabric-name-strings fname))
         (fname-string (apply string-append (interpose " " fname-strings)))
         (fnode (Node fname-string)))
    (set! fchar:node fnode)
    (set! fchar:name fname)
    fchar))
