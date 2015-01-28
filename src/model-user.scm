;;;; ***********************************************************************
;;;;
;;;; Name:          model-user.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       fabric user accounts
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export compute-random-salt defclass FabricUser)

(require "syntax-classes.scm")
(require "util-crypt.scm")

(defclass FabricUser ()
  (slots:
   (username init-form: #f getter: getUsername)
   (password init-form: "" getter: getPassword setter: setPassword)
   (salt init-form: (compute-random-salt) getter: getSalt setter: setSalt)))

