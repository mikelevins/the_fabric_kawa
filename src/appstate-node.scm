;;;; ***********************************************************************
;;;;
;;;; Name:          appstate-node.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the appstate for an in-game node
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export )

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "syntax-classes.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as AbstractAppState com.jme3.app.state.AbstractAppState)

;;; ---------------------------------------------------------------------
;;; the NodeAppState class
;;; ---------------------------------------------------------------------

(defclass NodeAppState (AbstractAppState)
  )
