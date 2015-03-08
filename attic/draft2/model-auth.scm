;;;; ***********************************************************************
;;;;
;;;; Name:          model-auth.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       representation of authentication and authorization data
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 AuthToken)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; 

;;; ---------------------------------------------------------------------
;;; required modules
;;; ---------------------------------------------------------------------

(require "util-java.scm")
(require "syntax-classes.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(import-as Serializable com.jme3.network.serializing.Serializable)

;;; CLASS AuthToken
;;; ---------------------------------------------------------------------
;;; the class of authentication tokens. An authentication token is a
;;; value that is passed from the Fabric Server to each client when
;;; the client sccessfully passes the authentication test. The server
;;; stores each token internally and also sends it to its client in a
;;; message. All client messages except requests to authenticate or to
;;; create an account must carry auth valid auth tokens, or they are
;;; automatically rejected. The server stores a timestamp and a
;;; time-to-live value with each auth token and updates the
;;; time-to-live periodically when it detects activity from the
;;; client. A client approaching the limit of its time-to-live is
;;; notified that its session will soon expire. If no further activity
;;; is detected before the time-to-live expires, the network
;;; connection is closed and the expired auth token is discarded.

(defclass AuthToken (java.lang.Object)
  (annotations: @Serializable)
  (slots:
   (username type: String init-form: #!null getter: getUsername setter: setUsername)
   (id type: String init-form: #!null getter: getId setter: setId)))
