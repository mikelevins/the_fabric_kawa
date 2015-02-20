;;;; ***********************************************************************
;;;;
;;;; Name:          model-auth.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       representation of authentication and authorization data
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export AuthToken)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; 

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "syntax-classes.scm")

;;; 
;;; ---------------------------------------------------------------------
;;; 

(defclass AuthToken (java.lang.Object)
  (slots:
   (username type: String init-form: #!null getter: getUsername setter: setUsername)
   (id type: String init-form: #!null getter: getId setter: setId)))
