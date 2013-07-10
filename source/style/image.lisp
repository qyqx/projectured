;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;
;;; Document

(def document style/image ()
  ((filename :type pathname)
   (raw :type t)))

;;;;;;
;;; Construction

(def (function e) make-style/image (filename)
  (make-instance 'style/image
                 :filename filename
                 :raw nil))

;;;;;;
;;; Construction

(def (macro e) style/image (filename)
  `(make-style/image ,filename))
