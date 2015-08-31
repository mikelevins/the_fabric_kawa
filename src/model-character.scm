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
 make-fabric-character)

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-error.scm")
(require "util-java.scm")
(require "util-lists.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as Node com.jme3.scene.Node)

;;; ---------------------------------------------------------------------
;;; FabricCharacter
;;; ---------------------------------------------------------------------

(define-simple-class FabricCharacter ()
  ;; slots
  ;; name
  (name init: #!null)
  ((getName) name)
  ((setName newname) (set! name newname))
  ;; node -- the JME node that represents the character in a scene
  (node init: #!null)
  ((getNode) node)
  ((setNode newnode) (set! node newnode))
  )

(define (make-fabric-character #!optional fabric-name)
  (let* ((fname (or fabric-name (random-fabric-name)))
         (fname-strings (fabric-name-strings fname))
         (fname-string (apply string-append (interpose " " fname-strings)))
         (fnode (Node fname-string))
         (fchar (FabricCharacter)))
    (*:setNode fchar fnode)
    (*:setName fchar fname)
    fchar))

