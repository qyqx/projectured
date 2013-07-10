;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;;;
;;;; Lisp form domain provides:
;;;;  - all types provided by the Common Lisp implementation

;;;;;;
;;; Lisp form document classes

(def document lisp-form/base ()
  ())

(def document lisp-form/comment (lisp-form/base)
  ((content :type string)))

(def document lisp-form/number (lisp-form/base)
  ((value :type number)))

(def document lisp-form/string (lisp-form/base)
  ((value :type string)))

(def document lisp-form/symbol (lisp-form/base)
  ((value :type symbol)
   (font-color :type style/color)))

(def document lisp-form/list (lisp-form/base)
  ((elements :type list)))

(def document lisp-form/object (lisp-form/base)
  ((value :type standard-object)))

;;;;;;
;;; Lisp form document constructors

(def (function e) make-lisp-form/comment (content)
  (make-instance 'lisp-form/comment :content content))

(def (function e) make-lisp-form/number (value)
  (make-instance 'lisp-form/number :value value))

(def (function e) make-lisp-form/string (value)
  (make-instance 'lisp-form/string :value value))

(def (function e) make-lisp-form/symbol (value &key font-color)
  (make-instance 'lisp-form/symbol :value value :font-color font-color))

(def (function e) make-lisp-form/list (elements)
  (make-instance 'lisp-form/list :elements elements))

(def (function e) make-lisp-form/object (value)
  (make-instance 'lisp-form/object :value value))
