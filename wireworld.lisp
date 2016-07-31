;; Brian Silverman's Wirewolrd

(ql:quickload "lispbuilder-sdl")

(defpackage :wireworld
  (:use :cl :sdl)
  (:export main))

(in-package :wireworld)

(defparameter *grid-width* 30)
(defparameter *grid-height* 30)
(defparameter *grid* nil)

(defparameter *window-width* 600)
(defparameter *window-height* 600)
(defparameter *window* nil)

(defun cell-width () (/ *window-width* *grid-width*))
(defun cell-height () (/ *window-height* *grid-height*))

(defparameter *cell-colors*
  '((empty         . sdl:*black*)
    (electron-head . sdl:*blue*)
    (electron-tail . sdl:*red*)
    (conductor     . sdl:*yellow*)))

(defun random-cell-state ()
  (let ((states (mapcar (lambda (cell) (car cell)) *cell-colors*)))
    (nth (random (length states)) states)))

(defun cell-color (cell-state)
  (cdr (assoc cell-state `((empty . ,sdl:*black*)
                           (electron-head . ,sdl:*blue*)
                           (electron-tail . ,sdl:*red*)
                           (conductor . ,sdl:*yellow*)))))

(defun over-grid-do (grid f)
  (loop
     for i from 0 to (- *grid-width* 1)
     do (loop
           for j from 0 to (- *grid-height* 1)
           do (funcall f grid i j))))

(defun init-grid ()
  (let ((grid (make-array (list *grid-width* *grid-height*))))
    (over-grid-do grid
                  (lambda (g x y)
                    (setf (aref g x y) (random-cell-state))))
    grid))

(defun empty-grid ()
  (make-array (list *grid-width* *grid-height*) :initial-element 'empty))

(defun read-cell-symbol (ch)
  (cdr (assoc ch '((#\. . empty)
                   (#\C . conductor)
                   (#\H . electron-head)
                   (#\T . electron-tail)))))

(defun load-initial-state (filename)
  (let ((grid (empty-grid))
        (in (open filename )))
    (when in
      (loop
         for y upfrom 0
         for line = (read-line in nil)
         while line do (loop
                          for c across line
                          for x upfrom 0 do (setf (aref grid x y) (read-cell-symbol c )))))
    grid))

(defun draw-cell (x y state)
  (let ((cw (cell-width))
        (ch (cell-height)))
    (fill-surface (cell-color state)
                  :template (rectangle :x (+ 1 (* x ch))
                                       :y (+ 1 (* y cw))
                                       :h (- ch 1)
                                       :w (- cw 1)))))

(defun render-grid (grid)
  (clear-display *black*)
  (over-grid-do grid (lambda (g x y) (draw-cell x y (aref g x y)))))

(defun main ()
  (setf *grid* (load-initial-state "w1.ww"))
  (with-init ()
    (setf *window*
          (window *window-width* *window-height*))
    (clear-display (sdl:color :r 150 :g 150 :b 150))
    (render-grid *grid*)
    (update-display)
    (with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
                       (case key
                         (:sdl-key-escape (push-quit-event)))))))
