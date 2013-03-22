;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :projectured)

;;;;;;;;
;;;; Graphics domain provides:
;;;;  - point, line, bezier, polyline, rectangle, polygon, circle, ellipse, arc, text, image, canvas
;;;;  - translation, scaling, rotation

;;;;;;
;;; 2d coordinate

(def (type e) 2d ()
  'complex)

(def (function e) make-2d (x y)
  (complex x y))

(def (function e) 2d-x (coordinate)
  (realpart coordinate))

(def (function e) 2d-y (coordinate)
  (imagpart coordinate))

;;;;;;
;;; 2d rectangle

(def class* rectangle ()
  ((location :type 2d)
   (size :type 2d)))

(def (function e) make-rectangle (location size)
  (make-instance 'rectangle :location location :size size))

(def (function e) rectangle-contains-point? (rectangle point)
  (bind ((location (location-of rectangle))
         (location-x (2d-x location))
         (location-y (2d-y location))
         (size (size-of rectangle))
         (size-x (2d-x size))
         (size-y (2d-y size)))
    (and (<= location-x (2d-x point) (+ location-x size-x))
         (<= location-y (2d-y point) (+ location-y size-y)))))

(def (function e) rectangle-union (rectangle-1 rectangle-2)
  (bind ((location-1 (location-of rectangle-1))
         (location-2 (location-of rectangle-2))
         (size-1 (size-of rectangle-1))
         (size-2 (size-of rectangle-2))
         (location (make-2d (min (2d-x location-1) (2d-x location-2))
                            (min (2d-y location-1) (2d-y location-2)))))
    (make-rectangle location
                    (- (make-2d (max (+ (2d-x location-1) (2d-x size-1)) (+ (2d-x location-2) (2d-x size-2)))
                                (max (+ (2d-y location-1) (2d-y size-1)) (+ (2d-y location-2) (2d-y size-2))))
                       location))))

;;;;;;
;;; Graphics API

(def (generic e) make-bounding-rectangle (instance)
  (:documentation "Returns a rectangle that contains all graphics of INSTANCE."))

(def (generic e) translate-location (instance translation)
  (:documentation "Translates the graphics element INSTANCE with a 2d TRANSLATION."))

(def (generic e) scale-size (instance scale)
  (:documentation "Scales the size of the graphics element INSTANCE with a 2d SCALE."))

(def (generic e) make-reference (instance location reference)
  (:documentation "Returns a reference describing the given LOCATION in the graphics element INSTANCE."))

;;;;;;
;;; Graphics document classes

(def document graphics/base ()
  ()
  (:documentation "Base class for graphics elements."))

(def document graphics/point (graphics/base)
  ((location :type 2d)
   (stroke-color :type vector)))

(def document graphics/line (graphics/base)
  ((begin :type 2d)
   (end :type 2d)
   (stroke-color :type vector)))

(def document graphics/rectangle (graphics/base)
  ((location :type 2d)
   (size :type 2d)
   (stroke-color :type vector)
   (fill-color :type vector)))

(def document graphics/polygon (graphics/base)
  ((points :type sequence)
   (stroke-color :type vector)
   (fill-color :type vector)))

(def document graphics/circle (graphics/base)
  ((center :type 2d)
   (radius :type number)
   (stroke-color :type vector)
   (fill-color :type vector)))

(def document graphics/ellipse (graphics/base)
  ((center :type 2d)
   (radius :type 2d)
   (stroke-color :type vector)
   (fill-color :type vector)))

(def document graphics/text (graphics/base)
  ((location :type 2d)
   (text :type string)
   (color :type vector)
   (font :type t)))

(def document graphics/image (graphics/base)
  ((location :type 2d)
   (source :type pathname)))

(def document graphics/viewport (graphics/base)
  ((content :type t)
   (location :type 2d)
   (size :type 2d)))

(def document graphics/canvas (graphics/base)
  ((location :type 2d)
   (elements :type sequence)))

;;;;;;
;;; Graphics document constructors

(def (function e) make-graphics/point (location &key stroke-color)
  (make-instance 'graphics/point :location location :stroke-color stroke-color))

(def (function e) make-graphics/line (begin end &key stroke-color)
  (make-instance 'graphics/line :begin begin :end end :stroke-color stroke-color))

(def (function e) make-graphics/rectangle (location size &key stroke-color fill-color)
  (make-instance 'graphics/rectangle :location location :size size :stroke-color stroke-color :fill-color fill-color))

(def (function e) make-graphics/polygon (points &key stroke-color fill-color)
  (make-instance 'graphics/polygon :points points :stroke-color stroke-color :fill-color fill-color))

(def (function e) make-graphics/circle (center radius &key stroke-color fill-color)
  (make-instance 'graphics/circle :center center :radius radius :stroke-color stroke-color :fill-color fill-color))

(def (function e) make-graphics/ellipse (center radius &key stroke-color fill-color)
  (make-instance 'graphics/ellipse :center center :radius radius :stroke-color stroke-color :fill-color fill-color))

(def (function e) make-graphics/text (location text &key color font)
  (make-instance 'graphics/text :location location :text text :color color :font font))

(def (function e) make-graphics/image (location source)
  (make-instance 'graphics/image :location location :source source))

(def (function e) make-graphics/viewport (content location size)
  (make-instance 'graphics/viewport :content content :location location :size size))

(def (function e) make-graphics/canvas (elements location)
  (make-instance 'graphics/canvas :elements elements :location location))

;;;;;;
;;; Graphics API implementation

(def methods make-bounding-rectangle
    (:method ((instance graphics/point))
      (make-rectangle (location-of instance) (make-2d 0 0)))

  (:method ((instance graphics/line))
    (bind ((begin (begin-of instance))
           (begin-x (2d-x begin))
           (begin-y (2d-y begin))
           (end (end-of instance))
           (end-x (2d-x end))
           (end-y (2d-y end))
           (top-left (make-2d (min begin-x end-x)
                              (min begin-y end-y)))
           (bottom-right (make-2d (max begin-x end-x)
                                  (max begin-y end-y))))
      (make-rectangle top-left (- bottom-right top-left))))

  (:method ((instance graphics/rectangle))
    (make-rectangle (location-of instance) (size-of instance)))

  (:method ((instance graphics/polygon))
    (iter (with points = (points-of instance))
          (for point :in-sequence points)
          (minimizing (2d-x point) :into min-x)
          (minimizing (2d-y point) :into min-y)
          (maximizing (2d-x point) :into max-x)
          (maximizing (2d-y point) :into max-y)
          (finally
           (return (bind ((top-left (make-2d min-x min-y))
                          (bottom-right (make-2d max-x max-y)))
                     (make-rectangle top-left (- bottom-right top-left)))))))

  (:method ((instance graphics/circle))
    (bind ((radius (radius-of instance))
           (size (* 2 radius)))
      (make-rectangle (- (center-of instance) (make-2d radius radius)) (make-2d size size))))

  (:method ((instance graphics/ellipse))
    (make-rectangle (- (center-of instance) (radius-of instance)) (* 2 (radius-of instance))))

  (:method ((instance graphics/text))
    (make-rectangle (location-of instance) (measure-text (text-of instance) (font-of instance))))

  #+nil
  (:method ((instance graphics/image))
    ;; TODO: no sdl here please, move this code
    (bind ((image (sdl-image:load-image (source-of instance) :color-key-at #(0 0)))
           (rectangle (sdl:get-surface-rect :surface image)))
      (make-rectangle (location-of instance) (make-2d (sdl:width rectangle) (sdl:height rectangle)))))

  (:method ((instance graphics/viewport))
    (make-rectangle (location-of instance) (size-of instance)))

  (:method ((instance graphics/canvas))
    (iter (with canvas-rectangle = (make-rectangle (make-2d 0 0) (make-2d 0 0)))
          (for element :in-sequence (elements-of instance))
          (for element-rectangle = (make-bounding-rectangle element))
          (setf canvas-rectangle (rectangle-union canvas-rectangle element-rectangle))
          (finally
           (incf (location-of canvas-rectangle) (location-of instance))
           (return canvas-rectangle)))))

(def methods translate-location
  (:method ((instance graphics/point) translation)
    (setf (location-of instance) (+ translation (location-of instance))))

  (:method ((instance graphics/line) translation)
    (setf (begin-of instance) (+ translation (begin-of instance)))
    (setf (end-of instance) (+ translation (end-of instance))))

  (:method ((instance graphics/rectangle) translation)
    (setf (location-of instance) (+ translation (location-of instance))))

  (:method ((instance graphics/polygon) translation)
    (setf (points-of instance)
          (iter (with points = (points-of instance))
                (for point :in-sequence points)
                (collect (+ point translation) :result-type 'vector))))

  (:method ((instance graphics/circle) translation)
    (setf (center-of instance) (+ translation (center-of instance))))

  (:method ((instance graphics/ellipse) translation)
    (setf (center-of instance) (+ translation (center-of instance))))

  (:method ((instance graphics/text) translation)
    (setf (location-of instance) (+ translation (location-of instance))))

  (:method ((instance graphics/image) translation)
    (setf (location-of instance) (+ translation (location-of instance))))

  (:method ((instance graphics/viewport) translation)
    (setf (location-of instance) (+ translation (location-of instance))))

  (:method ((instance graphics/canvas) translation)
    (not-yet-implemented)))

(def methods scale-size
  (:method ((instance graphics/point) scale)
    (values))

  (:method ((instance graphics/line) scale)
    (not-yet-implemented))

  (:method ((instance graphics/rectangle) scale)
    (not-yet-implemented))

  (:method ((instance graphics/polygon) scale)
    (not-yet-implemented))

  (:method ((instance graphics/circle) scale)
    (setf (radius-of instance) (* (radius-of instance)
                                  (/ (+ (2d-x scale) (2d-y scale)) 2))))

  (:method ((instance graphics/ellipse) scale)
    (setf (radius-of instance) (make-2d (* (2d-x (radius-of instance)) (2d-x scale))
                                        (* (2d-y (radius-of instance)) (2d-y scale)))))

  (:method ((instance graphics/text) scale)
    (not-yet-implemented))

  (:method ((instance graphics/image) scale)
    (not-yet-implemented))

  (:method ((instance graphics/canvas) scale)
    (not-yet-implemented)))

(def methods make-reference
  (:method :around ((instance graphics/base) location reference)
    (when (rectangle-contains-point? (make-bounding-rectangle instance) location)
      (call-next-method)))

  (:method ((instance graphics/point) location reference)
    nil)

  (:method ((instance graphics/line) location reference)
    nil)

  (:method ((instance graphics/rectangle) location reference)
    `(the ,(form-type instance) ,reference))

  (:method ((instance graphics/polygon) location reference)
    (not-yet-implemented))

  (:method ((instance graphics/circle) location reference)
    (not-yet-implemented))

  (:method ((instance graphics/ellipse) location reference)
    (not-yet-implemented))

  (:method ((instance graphics/text) location reference)
    ;; TODO: figure out which
    (iter (with text = (text-of instance))
          (with font = (font-of instance))
          (for index :from 0 :to (length text))
          (for size = (measure-text (subseq text 0 index) font))
          (when (rectangle-contains-point? (make-rectangle (location-of instance) size) location)
            (return `(the character (elt (the string (text-of (the ,(form-type instance) ,reference))) ,(1- index)))))))

  (:method ((instance graphics/image) location reference)
    (not-yet-implemented))

  (:method ((instance graphics/viewport) location reference)
    (make-reference (content-of instance) (- location
                                             (location-of instance)
                                             (location-of (content-of instance)))
                    `(content-of (the ,(form-type instance) ,reference))))

  (:method ((instance graphics/canvas) location reference)
    (iter (for index :from 0)
          (for element :in (elements-of instance))
          (thereis (make-reference element (- location (location-of instance))
                                   `(elt (the list (elements-of (the ,(form-type instance) ,reference))) ,index))))))

;;;;;;
;;; Graphics operation classes

(def operation operation/graphics/translate-location (operation)
  ((selection :type selection/single)
   (translation :type 2d)))

(def operation operation/graphics/scale-size (operation)
  ((selection :type selection/single)
   (scale :type 2d)))

(def operation operation/graphics/rotate-clockwise (operation)
  ((selection :type selection/single)
   (rotation :type number)))

;;;;;;
;;; Graphics operation constructors

(def (function e) make-operation/graphics/translate-location (selection translation)
  (make-instance 'operation/graphics/translate-location :selection selection :translation translation))

(def (function e) make-operation/graphics/scale-size (selection scale)
  (make-instance 'operation/graphics/scale-size :selection selection :scale scale))

(def (function e) make-operation/graphics/rotate-clockwise (selection rotation)
  (make-instance 'operation/graphics/rotate-clockwise :selection selection :rotation rotation))

;;;;;;
;;; Graphics operation API implementation

(def method redo-operation ((operation operation/graphics/translate-location))
  (translate-location (target-of (selection-of operation)) (translation-of operation)))

(def method redo-operation ((operation operation/graphics/scale-size))
  (scale-size (target-of (selection-of operation)) (scale-of operation)))

(def method redo-operation ((operation operation/graphics/rotate-clockwise))
  (not-yet-implemented))
