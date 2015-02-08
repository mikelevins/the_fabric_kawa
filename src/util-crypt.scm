;;;; ***********************************************************************
;;;;
;;;; Name:          util-crypt.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       cryptographic utilities
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export base64->digest compute-random-salt text->digest)

(require "util-lists.scm")
(require "util-java.scm")
(require "util-random.scm")

(import-as Base64 java.util.Base64)
(import-as PBEKeySpec javax.crypto.spec.PBEKeySpec)
(import-as Random java.security.SecureRandom)
(import-as SecretKeyFactory javax.crypto.SecretKeyFactory)
(import-as String java.lang.String)

(define (compute-random-salt)
  (let ((rand (Random:getInstance "SHA1PRNG"))
        (bytes (byte[] length: 8)))
    (*:nextBytes rand bytes)
    (array->list bytes)))

(define (compute-salted-digest text::String salt)
  (let* ((salt-bytes (apply byte[] salt))
         (iterations 10000)
         (digest-length 160)
         (algo "PBKDF2WithHmacSHA1")
         (spec (PBEKeySpec (*:toCharArray text)
                           salt-bytes
                           iterations
                           digest-length))
         (factory (SecretKeyFactory:getInstance algo)))
    (array->list (*:getEncoded (*:generateSecret factory spec)))))

(define (digest->base64 digest::byte[])
  (let* ((encoder (Base64:getEncoder))
         (bytes::byte[] (*:encode encoder digest)))
    (java.lang.String bytes)))

(define (base64->digest text::java.lang.String)
  (let* ((decoder (Base64:getDecoder)))
    (*:decode decoder text)))

(define (text->digest text salt)
  (let* ((bytes (compute-salted-digest text salt))
         (byte-array (apply byte[] bytes)))
    (digest->base64 byte-array)))


