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

(def document walked-lisp-form/base ()
  ())

;; KLUDGE:
(def document walked-lisp-form/comment (walked-lisp-form/base hu.dwim.walker::walked-form)
  ((content :type string)))

;;;;;;
;;; Walked lisp form constructors
;;;
;;; KLUDGE:
;;; The walked lisp form document constructores are provided by the :hu.dwim.walker project.

(def (function e) make-walked-lisp-form/comment (content)
  (make-instance 'walked-lisp-form/comment :content content))
