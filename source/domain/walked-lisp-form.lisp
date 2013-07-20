;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;;;
;;;; Walked lisp form domain provides:
;;;;  - all form classes provided by :hu.dwim.walker

;;;;;;
;;; Walked lisp form document classes
;;;
;;; KLUDGE:
;;; The walked lisp form document classes are provided by the :hu.dwim.walker project.

;; TODO: add all forms from hu.dwim.walker project

(def document common-lisp/base ()
  ((indentation :type integer)))

(def document common-lisp/constant (common-lisp/base)
  ())

(def document common-lisp/variable-reference (common-lisp/base)
  ())

(def document common-lisp/if (common-lisp/base)
  ())

(def document common-lisp/progn (common-lisp/base)
  ())

(def document common-lisp/the (common-lisp/base)
  ())

(def document common-lisp/lexical-variable-binding (common-lisp/base)
  ())

(def document common-lisp/let (common-lisp/base)
  ())

(def document common-lisp/application (common-lisp/base)
  ())

(def document common-lisp/function-definition (common-lisp/base)
  ())

(def document common-lisp/lambda-function (common-lisp/base)
  ())

(def document common-lisp/function-argument (common-lisp/base)
  ())

;; KLUDGE:
(def document walked-lisp-form/base ()
  ((indentation :type integer)))

(def document walked-lisp-form/comment (walked-lisp-form/base hu.dwim.walker::walked-form)
  ((content :type string)))

(def document walked-lisp-form/top-level-forms (walked-lisp-form/base)
  ((body :type sequence)))

;;;;;;
;;; Walked lisp form constructors
;;;
;;; KLUDGE:
;;; The walked lisp form document constructores are provided by the :hu.dwim.walker project.

(def (function e) make-walked-lisp-form/comment (content)
  (make-instance 'walked-lisp-form/comment :content content))

(def (function e) make-walked-lisp-form/top-level-forms (body)
  (make-instance 'walked-lisp-form/top-level-forms :body body))
