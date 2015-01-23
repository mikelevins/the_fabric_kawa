;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          app-client.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       the game client
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require 'list-lib)
(require "util-java.scm")
(require "syntax-classes.scm")
(require "util-general.scm")

;;; ---------------------------------------------------------------------
;;; imports
;;; ---------------------------------------------------------------------

(import-as BorderLayout java.awt.BorderLayout)
(import-as Dimension java.awt.Dimension)
(import-as JFrame javax.swing.JFrame)
(import-as JLabel javax.swing.JLabel)

;;; ---------------------------------------------------------------------
;;; construct the client app
;;; ---------------------------------------------------------------------

(define (make-client)
  (let* ((client-frame :: JFrame (JFrame "The Fabric"))
         (username-label :: JLabel (JLabel "username:")))
    (*:setPreferredSize username-label (Dimension 200 40))
    (*:add (*:getContentPane client-frame)
           username-label
           BorderLayout:CENTER)
    (*:pack client-frame)
    (*:setVisible client-frame #t)
    client-frame))


