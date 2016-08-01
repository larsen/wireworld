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
                          for x upfrom 0 do (setf (aref grid x y)
                                                  (read-cell-symbol c)))))
    grid))

(defun up (idx) (if (= 0 idx) (- *grid-height* 1) (- idx 1)))
(defun down (idx) (if (= (- *grid-height* 1) idx) 0 (+ idx 1)))
(defun left (idx) (if (= 0 idx) (- *grid-width* 1) (- idx 1)))
(defun right (idx) (if (= (- *grid-width* 1) idx) 0 (+ idx 1)))

(defun moore-neighbours ()
  `((,#'up   ,#'left)
    (,#'up   nil)
    (,#'up   ,#'right)
    (nil     ,#'left)
    (nil     ,#'right)
    (,#'down ,#'left)
    (,#'down nil)
    (,#'down ,#'right)))

(defun neighbours (grid x y neighbourhood-function)
  (flet ((get-status (x y) (aref grid x y)))
    (loop for n in (funcall neighbourhood-function)
       collect (get-status
                (if (car n) (funcall (car n) x)
                    x)
                (if (cadr n)
                    (funcall (cadr n) y)
                    y)))))

(defun count-electron-heads-neighbours (grid x y)
  (count-if
   (lambda (state) (equal state 'electron-head))
   (neighbours grid x y #'moore-neighbours)))

(defun new-cell-status (grid x y)
  (let ((old-state (aref grid x y)))
    (case old-state
      ('empty 'empty)
      ('electron-head 'electron-tail)
      ('electron-tail 'conductor)
      ('conductor
       (let ((electron-head-neighbours (count-electron-heads-neighbours grid x y)))
         (if (or (= electron-head-neighbours 1)
                 (= electron-head-neighbours 2))
             'electron-head
             'conductor))))))

(defun update-grid (grid)
  (let ((tmp-grid (make-array (list *grid-width* *grid-height*))))
    (over-grid-do tmp-grid
                  (lambda (g x y) (setf (aref g x y) (new-cell-status grid x y))))
    (setf *grid* tmp-grid)))

(defun draw-cell (x y state)
  (let ((cw (cell-width))
        (ch (cell-height)))
    (fill-surface (cell-color state)
                  :template (rectangle :x (+ 1 (* x ch))
                                       :y (+ 1 (* y cw))
                                       :h (- ch 1)
                                       :w (- cw 1)))))

(defun coord-to-cell (x y)
  (list (floor (/ x (cell-width)))
        (floor (/ y (cell-height)))))

(defun render-grid (grid)
  (clear-display (sdl:color :r 50 :g 50 :b 50))
  (over-grid-do grid (lambda (g x y) (draw-cell x y (aref g x y)))))

(defvar *paused* nil)
(defvar *user-insert-element* 'conductor)

(defun message (msg)
  (let ((rendered-msg (format nil "~30a" msg)))
    (sdl:draw-surface-at-*
     (sdl:render-string-shaded rendered-msg sdl:*yellow* sdl:*black*
                               :cache t
                               :font (sdl:initialise-default-font))
     1 1))
  (update-display))

(defun empty-message ()
  (message ""))

(defun set-user-insert-element (element)
  (setf *user-insert-element* element)
  (message (string element)))

(defun toggle-pause ()
  (setf *paused* (if (eq *paused* t) nil t)))

(defun main ()
  (setf *grid* (load-initial-state "mult.ww"))
  (with-init ()
    (setf *window*
          (window *window-width* *window-height*))
    (clear-display (sdl:color :r 50 :g 50 :b 50))
    (render-grid *grid*)
    (update-display)
    (with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
                       (case key
                         (:sdl-key-escape (push-quit-event))
                         (:sdl-key-space (toggle-pause)
                                         (empty-message))
                         (:sdl-key-c (set-user-insert-element 'conductor))
                         (:sdl-key-h (set-user-insert-element 'electron-head))
                         (:sdl-key-t (set-user-insert-element 'electron-tail))
                         (:sdl-key-e (set-user-insert-element 'empty))
                         (:sdl-key-s (if *paused*
                                         (progn
                                           (update-grid *grid*)
                                           (render-grid *grid*)
                                           (update-display))))))
      (:mouse-button-down-event (:x x :y y)
                                (when (sdl:mouse-left-p)
                                  (destructuring-bind (cell-x cell-y)
                                      (coord-to-cell x y)
                                    (progn (setf (aref *grid* cell-x cell-y) *user-insert-element*)
                                           (render-grid *grid*)
                                           (update-display)))))
      (:idle (unless *paused*
               (update-grid *grid*)
               (render-grid *grid*)
               (update-display))))))
