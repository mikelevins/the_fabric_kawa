;;;; ***********************************************************************
;;;;
;;;; Name:          model-rect.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       rectangle structures
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 get-left
 get-top
 get-width
 get-height
 make-rectangle
 <rectangle>
 rectangle?)

(define-record-type <rectangle> 
  (make-rectangle left top width height)
  rectangle?
  (left get-left)
  (top get-top)
  (width get-width)
  (height get-height))
