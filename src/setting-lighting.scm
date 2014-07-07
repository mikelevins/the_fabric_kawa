;;;; ***********************************************************************
;;;; Name:          setting-lighting.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       set up the scene lighting effects
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export init-lighting)

(require "assets-general.scm")
(require "interface-frame.scm")

(define-private-alias AssetManager com.jme3.asset.AssetManager)
(define-private-alias BloomFilter com.jme3.post.filters.BloomFilter)
(define-private-alias FilterPostProcessor com.jme3.post.FilterPostProcessor)
(define-private-alias ViewPort com.jme3.renderer.ViewPort)

(define (init-lighting app)
  (let* ((asset-manager::AssetManager (get-asset-manager))
         (bloom (BloomFilter BloomFilter:GlowMode:Objects))
         (filter-processor::FilterPostProcessor (FilterPostProcessor asset-manager))
         (viewport::ViewPort (get-key app viewport:)))
    (*:setDownSamplingFactor bloom 2.0)
    (*:setBloomIntensity bloom 2.0)
    (*:addFilter filter-processor bloom)
    (*:addProcessor viewport filter-processor)))

