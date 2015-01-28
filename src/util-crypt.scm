;;;; ***********************************************************************
;;;;
;;;; Name:          util-crypt.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       cryptographic utilities
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export compute-random-salt text->digest)

(require "util-java.scm")
(require "util-random.scm")

(import-as Base64 java.util.Base64)
(import-as PBEKeySpec javax.crypto.spec.PBEKeySpec)
(import-as Random java.security.SecureRandom)
(import-as SecretKeyFactory javax.crypto.SecretKeyFactory)

(define (compute-random-salt)
  (let ((rand (Random:getInstance "SHA1PRNG"))
        (bytes (byte[] length: 8)))
    (*:nextBytes rand bytes)
    bytes))

(define (compute-salted-digest text salt)
  (let* ((iterations 10000)
         (digest-length 160)
         (algo "PBKDF2WithHmacSHA1")
         (spec (PBEKeySpec (*:toCharArray text)
                           salt
                           iterations
                           digest-length))
         (factory (SecretKeyFactory:getInstance algo)))
    (*:getEncoded (*:generateSecret factory spec))))

(define (digest->base64 digest::byte[])
  (let* ((encoder (Base64:getEncoder))
         (bytes::byte[] (*:encode encoder digest)))
    (java.lang.String bytes)))

(define (base64->digest text::java.lang.String)
  (let* ((decoder (Base64:getDecoder)))
    (*:decode decoder text)))

(define (text->digest text salt)
  (digest->base64 (compute-salted-digest text salt)))
