;;;; Base64 Encoding/Decoding for Common Lisp
;;;;
;;;; Copyright (c) Jeffrey Massung
;;;;
;;;; This file is provided to you under the Apache License,
;;;; Version 2.0 (the "License"); you may not use this file
;;;; except in compliance with the License.  You may obtain
;;;; a copy of the License at
;;;;
;;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing,
;;;; software distributed under the License is distributed on an
;;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;;; KIND, either express or implied.  See the License for the
;;;; specific language governing permissions and limitations
;;;; under the License.
;;;;

(defpackage :base64
  (:use :cl)
  (:export
   #:base64-encode
   #:base64-decode))

(in-package :base64)

;;; ----------------------------------------------------

(defparameter *base64*
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

;;; ----------------------------------------------------

(defun make-octet (b1 &optional b2 b3)
  "Create a 3-byte octet from 1, 2, or 3 characters."
  (flet ((shift (c s)
           (ash (if c (char-code c) 0) s)))
    (logior (shift b1 16)
            (shift b2 8)
            (shift b3 0))))

;;; ----------------------------------------------------

(defun make-quartet (b1 b2 b3 b4)
  "Create a 3-byte quartet from 2, 3, or 4 encoded characters."
  (flet ((shift (c s)
           (ash (if (char= c #\=) 0 (position c *base64* :test #'char=)) s)))
    (logior (shift b1 18)
            (shift b2 12)
            (shift b3 6)
            (shift b4 0))))

;;; ----------------------------------------------------

(defun encode-octet (out b1 &optional b2 b3 &aux (p (make-octet b1 b2 b3)))
  "Writes the next four characters to the output stream."
  (let ((a (char *base64* (ash (logand p #xFC0000) -18)))
        (b (char *base64* (ash (logand p #x03F000) -12)))
        (c (char *base64* (ash (logand p #x000FC0) -6)))
        (d (char *base64*      (logand p #x00003F))))
    (cond
     ((null b2) (format out "~c~c==" a b))
     ((null b3) (format out "~c~c~c=" a b c))
     (t         (format out "~c~c~c~c" a b c d)))))

;;; ----------------------------------------------------

(defun decode-octet (out b1 b2 b3 b4 &aux (p (make-quartet b1 b2 b3 b4)))
  "Writes the next three characters to the output stream."
  (let ((a (code-char (logand (ash p -16) #xff)))
        (b (code-char (logand (ash p -8)  #xff)))
        (c (code-char (logand      p      #xff))))
    (cond
     ((char= b3 #\=) (format out "~c" a))
     ((char= b4 #\=) (format out "~c~c" a b))
     (t              (format out "~c~c~c" a b c)))))

;;; ----------------------------------------------------

(defun base64-encode (string)
  "Encode a simple string of base-chars into a base64 string."
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop
         for c1 = (read-char in nil nil)
         for c2 = (read-char in nil nil)
         for c3 = (read-char in nil nil)
         until (null c1)
         do (encode-octet out c1 c2 c3)))))

;;; ----------------------------------------------------

(defun base64-decode (string)
  "Decode a string into a simple string."
  (with-output-to-string (out)
    (with-input-from-string (in string)
      (loop
         for i from 1 to (/ (length string) 4)
         do (decode-octet out
                          (read-char in)
                          (read-char in)
                          (read-char in)
                          (read-char in))))))
