;;;; ***********************************************************************
;;;;
;;;; Name:          model-frame.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       Frame implementations
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export AListFrame)

(require 'list-lib)
(require "interfaces-frame.scm")

;;; ---------------------------------------------------------------------
;;; AListFrame
;;; ---------------------------------------------------------------------

(define-simple-class AListFrame (IMutableFrame)
  ;;; slots
  (frame-state init-form: '())
  ;;; accessors
  ((getFrameState) frame-state)
  ((setFrameState st) (set! frame-state st))
  ;;; implementations
  ((getKey key) (let ((entry (assoc key frame-state)))
                  (if entry (cdr entry) #f)))
  ((putKey key val) (let ((entry (assoc key frame-state)))
                      (if entry
                          (let ((new-frame (AListFrame)))
                            (*:setFrameState new-frame
                                             (cons (cons key val)
                                                   (remove (lambda (e)(equal? key (car e)))
                                                           (*:getFrameState (this)))))
                            new-frame)
                          (let ((new-frame (AListFrame)))
                            (*:setFrameState new-frame
                                             (cons (cons key val)
                                                   (*:getFrameState (this))))
                            new-frame))))
  ((removeKey key) (let ((new-frame (AListFrame)))
                     (*:setFrameState new-frame
                                      (remove (lambda (e)(equal? key (car e)))
                                              (*:getFrameState (this))))
                     new-frame))
  ((containsKey key) (and (assoc key (*:getFrameState (this))) #t))
  ((listKeys) (map car (*:getFrameState (this))))
  ((isEmpty) (null? (*:getFrameState (this))))
  ((setKey key val) (let ((entry (assoc key frame-state)))
                      (if entry
                          (set-cdr! entry val)
                          (*:setFrameState (this)
                                           (cons (cons key val)
                                                 (*:getFrameState (this)))))
                      (this)))
  ((deleteKey key) (begin
                     (*:setFrameState (this)
                                      (remove (lambda (e)(equal? key (car e)))
                                              (*:getFrameState (this))))
                     (this))))

