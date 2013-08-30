(defpackage :base64-asd
  (:use :cl :asdf))

(in-package :base64-asd)

(defsystem :base64
  :name "base64"
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "Base64 encoding and decoding for Common Lisp."
  :serial t
  :components ((:file "base64")))
