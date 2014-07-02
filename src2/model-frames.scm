;;;; ***********************************************************************
;;;; Name:          model-pmap.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       simple property maps
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export frame? contains-key? contains-val?
               get-key put-key set-key! remove-key delete-key! merge-keys frame-keys frame-vals)

(require "utilities-java.scm")

(defgeneric frame?)

(defmethod (frame? x) #f)

(defgeneric contains-key?)
(defgeneric contains-val?)
(defgeneric get-key)
(defgeneric put-key)
(defgeneric set-key!)
(defgeneric remove-key)
(defgeneric delete-key!)
(defgeneric frame-keys)
