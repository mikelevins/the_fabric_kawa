;;; ***********************************************************************
;;;;
;;;; Name:          syntax-events.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       syntax definitions for event-handling
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 on-action
 on-analog)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; This file implements macros that simplify and clarify code that
;;; sets up event handling in the Fabric's JME3 classes.

;;; MACRO (on-analog (evt-name)(s -> expr) ...)
;;; ---------------------------------------------------------------------
;;; creates event handlers for the analog event named _evt-name_,
;;; where each event-type identifier _s_ is mapped to a handler
;;; _expr_, which may be any arbitrary Scheme expression. When an
;;; event _evt-name_ of type _s_ is signalled, _expr_ is executed.

(define-syntax on-analog
  (syntax-rules (->)
    ((on-analog (evt-name)
                (s -> expr) ...)
     (cond
      ((invoke evt-name 'equals s) expr) ...
      (#t #f)))))


;;; (on-action (evt-name)(s -> expr) ...)
;;; ---------------------------------------------------------------------
;;; creates event handlers for the action event named _evt-name_,
;;; where each event-type identifier _s_ is mapped to a handler
;;; _expr_, which may be any arbitrary Scheme expression. When an
;;; event _evt-name_ of type _s_ is signalled, _expr_ is executed.

(define-syntax on-action
  (syntax-rules (->)
    ((on-action (evt-name)
                (s -> expr) ...)
     (cond
      ((invoke evt-name 'equals s) expr) ...
      (#t #f)))))

