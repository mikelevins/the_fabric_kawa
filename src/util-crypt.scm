;;;; ***********************************************************************
;;;;
;;;; Name:          util-crypt.scm
;;;; Project:       The Fabric: a far-future MMORPG
;;;; Purpose:       cryptographic utilities
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 base64->digest
 compute-random-salt
 text->digest)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; This file provides cryptographic utilities used by the Fabric,
;;; including cryptographic hashing for passwords and other sensitive
;;; data.

(require "util-lists.scm")
(require "util-java.scm")
(require "util-random.scm")

(import-as Base64 java.util.Base64)
(import-as PBEKeySpec javax.crypto.spec.PBEKeySpec)
(import-as Random java.security.SecureRandom)
(import-as SecretKeyFactory javax.crypto.SecretKeyFactory)
(import-as String java.lang.String)

;;; (compute-random-salt)
;;; ---------------------------------------------------------------------
;;; returns a list of randomly-chosen bytes using the SHAPRNG1
;;; algorithm for generating cryptographically-useful salts suitable
;;; for computing digests

(define (compute-random-salt)
  (let ((rand (Random:getInstance "SHA1PRNG"))
        (bytes (byte[] length: 8)))
    (*:nextBytes rand bytes)
    (array->list bytes)))

;;; (compute-salted-digest text::String salt)
;;; ---------------------------------------------------------------------
;;; returns a list of bytes representing a salted digest of _text_
;;; computed using the "PBKDF2WithHmacSHA1" algorithm

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

;;; (digest->base64 digest::byte[])
;;; ---------------------------------------------------------------------
;;; returns a string representing a base-64 encoding of _digest_

(define (digest->base64 digest::byte[])
  (let* ((encoder (Base64:getEncoder))
         (bytes::byte[] (*:encode encoder digest)))
    (java.lang.String bytes)))

;;; (base64->digest text::java.lang.String)
;;; ---------------------------------------------------------------------
;;; returns the decoded digest bytes represented by a base-64-encoded
;;; string

(define (base64->digest text::java.lang.String)
  (let* ((decoder (Base64:getDecoder)))
    (*:decode decoder text)))

;;; (text->digest text salt)
;;; ---------------------------------------------------------------------
;;; returns a base-64-encoded string representation of the
;;; cryptographic digest of _text_, using _salt_ as the cryptographic
;;; salt used in computing the digest. se this function to convert
;;; cleartext password strings to hashed digests for storage and
;;; comparisons.

(define (text->digest text salt)
  (let* ((bytes (compute-salted-digest text salt))
         (byte-array (apply byte[] bytes)))
    (digest->base64 byte-array)))


