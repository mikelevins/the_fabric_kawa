;;;; ***********************************************************************
;;;;
;;;; Name:          model-properties.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       using Java properties files for storage
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export )

(require "klos.scm")
(require "syntax-classes.scm")

(defgeneric object->property-string)

(defmethod object->property-string ((ls gnu.lists.LList))
  (format #f "~s" ls))

(define (property-string->object str)
  (call-with-input-string str (lambda (in)(read in))))
