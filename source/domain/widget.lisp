;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;;;
;;;; Widget domain provides:
;;;;  - checkbox
;;;;  - button
;;;;  - text field

;;;;;;
;;; Data structure

(def document widget/base ()
  ((visible :type boolean)))

(def document widget/checkbox (widget/base)
  ())

(def document widget/button (widget/base)
  ())

(def document widget/text-field (widget/base)
  ())

(def document widget/tooltip (widget/base)
  ((location :type 2d)
   (content :type t)))

(def document widget/composite (widget/base)
  ((elements :type sequence)))

(def document widget/scroll-pane (widget/base)
  ((content :type t)
   (scroll-position :type 2d)))

;;;;;;
;;; Construction

(def (function e) make-widget/tooltip (location content)
  (make-instance 'widget/tooltip
                 :visible #f
                 :location location
                 :content content))

(def (function e) make-widget/composite (elements)
  (make-instance 'widget/composite :elements elements))

(def (function e) make-widget/scroll-pane (content)
  (make-instance 'widget/scroll-pane
                 :content content
                 :scroll-position (as (make-2d 0 0))))

;;;;;;
;;; Construction

(def (macro e) tooltip ((&key location) &body content)
  `(make-widget/tooltip ,location ,(first content)))

(def (macro e) composite ((&key) &body elements)
  `(make-widget/composite (list ,@elements)))

(def (macro e) scroll-pane ((&key) &body content)
  `(make-widget/scroll-pane ,(first content)))

;;;;;;
;;; Operation data structure

(def operation operation/widget/hide (operation)
  ((widget :type widget/base)))

(def operation operation/widget/show (operation)
  ((widget :type widget/base)))

(def operation operation/widget/tooltip/move (operation)
  ((tooltip :type tooltip)
   (location :type location)))

(def operation operation/widget/tooltip/replace-content (operation)
  ((tooltip :type tooltip)
   (content :type content)))

(def operation operation/widget/scroll-pane/scroll (operation)
  ((scroll-pane :type widget/scroll-pane)
   (scroll-delta :type 2d)))

;;;;;;
;;; Redo

(def method redo-operation ((operation operation/widget/hide))
  (setf (visible-p (widget-of operation)) #f))

(def method redo-operation ((operation operation/widget/show))
  (setf (visible-p (widget-of operation)) #t))

(def method redo-operation ((operation operation/widget/tooltip/move))
  (setf (location-of (tooltip-of operation)) (location-of operation)))

(def method redo-operation ((operation operation/widget/tooltip/replace-content))
  (setf (content-of (tooltip-of operation)) (content-of operation)))

(def method redo-operation ((operation operation/widget/scroll-pane/scroll))
  (bind ((scroll-pane (scroll-pane-of operation)))
    (setf (scroll-position-of scroll-pane) (+ (scroll-position-of scroll-pane)
                                              (scroll-delta-of operation)))))
