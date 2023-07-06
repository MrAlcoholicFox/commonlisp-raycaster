;;;; Le pisscaster

;;Poss check translate shit etc

;;; lib
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :lispbuilder-sdl)
  (ql:quickload :lispbuilder-sdl-image))

;;; Some general parameters
(defparameter *mapwidth* 24)
(defparameter *mapheight* 24)
(defparameter *screenwidth* 640)
(defparameter *screenheight* 480)
(defparameter *half-height* (/ *screenheight* 2))
(defparameter *half-width* (/ *screenwidth* 2))
(defparameter *fps* 30)
(defparameter *ang-increment* nil) ; Set later as it relies on player fov
(defparameter *ray-precision* 64)
(defparameter *scale* 1) ;probably tweak a bit

;;; The surface that everything is gonna be drawn to before being zoomed (despair)
(defparameter *surface* nil)

;;; Projection parameters
(defparameter *proj-width* (/ *screenwidth* *scale*))
(defparameter *proj-height* (/ *screenheight* *scale*))
(defparameter *proj-hlf-width* (/ *proj-width* 2))
(defparameter *proj-hlf-height* (/ *proj-height* 2))


(defparameter *worldmap*
  #2A((1 1 1 1 1 1 1 1 1 1)
      (1 0 0 0 0 0 0 0 0 1)
      (1 0 0 0 0 0 0 0 0 1)
      (1 0 0 1 1 0 1 0 0 1)
      (1 0 0 1 0 0 1 0 0 1)
      (1 0 0 1 0 0 1 0 0 1)
      (1 0 0 1 0 1 1 0 0 1)
      (1 0 0 0 0 0 0 0 0 1)
      (1 0 0 0 0 0 0 0 0 1)
      (1 1 1 1 1 1 1 1 1 1)))

;;; Textures
(defstruct texture width height bitmap colors) ; probably fix the color thing later lmao, can probs convert to list

(defparameter *brick* (make-texture
		       :width 8
		       :height 8
		       :bitmap #2A((1 1 1 1 1 1 1 1)
				   (0 0 0 1 0 0 0 1)
				   (1 1 1 1 1 1 1 1)
				   (0 1 0 0 0 1 0 0)
				   (1 1 1 1 1 1 1 1)
				   (0 0 0 1 0 0 0 1)
				   (1 1 1 1 1 1 1 1)
				   (0 1 0 0 0 1 0 0))
		       :colors (list (sdl:color :r 255 :g 241 :b 232) (sdl:color :r 194 :g 195 :b 199))))

;(defparameter *brick2* (make-texture
;			:width 16
;			:height 16))
			

(defparameter *textures* (list *brick*))

;;; The player struct and global player variable (imagine using classes lmao)
(defstruct player posx posy angle fov halffov movement rotation)
(defparameter *player* nil)

(defmacro degree-to-rad (deg)
  `(* (/ pi 180) ,deg))

(defmacro cos-degrees (deg)
  `(cos (degree-to-rad ,deg)))

(defmacro sin-degrees (deg)
  `(sin (degree-to-rad ,deg)))

(defmacro pythagoras (a b)
  `(sqrt (+ (expt ,a 2) (expt ,b 2))))

;;; Player logic shit
(defun move-forward ()
  (let ((new-x (+ (player-posx *player*) (* (cos-degrees (player-angle *player*)) (player-movement *player*))))
	(new-y (+ (player-posy *player*) (* (sin-degrees (player-angle *player*)) (player-movement *player*)))))
    (when (= (aref *worldmap* (floor new-y) (floor new-x)) 0)
      (setf (player-posx *player*) new-x)
      (setf (player-posy *player*) new-y))))	

(defun move-backward ()
  (let ((new-x (- (player-posx *player*) (* (cos-degrees (player-angle *player*)) (player-movement *player*))))
	(new-y (- (player-posy *player*) (* (sin-degrees (player-angle *player*)) (player-movement *player*)))))
    (when (= (aref *worldmap* (floor new-y) (floor new-x)) 0)
	(setf (player-posx *player*) new-x)
	(setf (player-posy *player*) new-y))))

(defun look-left ()
  (decf (player-angle *player*) (player-rotation *player*)))

(defun look-right ()
  (incf (player-angle *player*) (player-rotation *player*)))

(defun keyhandler ()
  (when (sdl:key-down-p :sdl-key-w)
    (move-forward))

  (when (sdl:key-down-p :sdl-key-s)
    (move-backward))

  (when (sdl:key-down-p :sdl-key-a)
    (look-left))

  (when (sdl:key-down-p :sdl-key-d)
    (look-right)))

(defun drawtexture (x wallheight texture-posx txture)
  (let ((y-incrementer (/ (* wallheight 2) (texture-height txture)))
	(y (- *proj-hlf-height* wallheight)))
    (dotimes (i (texture-height txture))
      (let ((color (nth (aref (texture-bitmap txture) i texture-posx) (texture-colors txture))))
	(sdl:draw-line (sdl:point :x x :y y)
		       (sdl:point :x x :y (+ y (+ y-incrementer 0.5))) :color color :surface *surface*)
	(incf y y-incrementer)))))

(defun raycasting ()
  (let ((rayangle (- (player-angle *player*) (player-halffov *player*))))
	
    (dotimes (raycount *proj-width*)
      (let ((raycos (/ (cos-degrees rayangle) *ray-precision*))
	    (raysin (/ (sin-degrees rayangle) *ray-precision*))
	    (x (player-posx *player*))
	    (y (player-posy *player*))
	    (wall 0))
	
	(loop do
	  (incf x raycos)
	  (incf y raysin)
	  (if (>= y 10) (setf y 9)) ;stupid floating shit
	  (if (>= x 10) (setf x 9)) ;stupid floating shit
	 ; (format t "X: ~A Y: ~A RAYCOS: ~A RAYSIN: ~A~%" x y raycos raysin)
	  (setf wall (aref *worldmap* (floor y) (floor x)))
	      while (= wall 0))

	(let* ((distance (pythagoras (- (player-posx *player*) x) (- (player-posy *player*) y)))
	       (distance (* distance (cos-degrees (- rayangle (player-angle *player*))))) ;fisheye fix
	       (wallheight (floor (/ *proj-hlf-height* distance)))
	       (txture (nth (- wall 1) *textures*))
	       (texture-posx (floor (mod (* (texture-width txture) (+ x y)) (texture-width txture)))))

	  ;;Draw the shit yass
	  (sdl:draw-line (sdl:point :x raycount :y 0)
			 (sdl:point :x raycount :y (- *proj-hlf-height* wallheight)) :color sdl:*black*
										     :surface *surface*)
	  
	  #| (sdl:draw-line (sdl:point :x raycount :y (- *proj-hlf-height* wallheight))
			 (sdl:point :x raycount :y (+ *proj-hlf-height* wallheight)) :color sdl:*red*
	  :surface *surface*) |#

	  (drawtexture raycount wallheight texture-posx txture)

	  (sdl:draw-line (sdl:point :x raycount :y (+ *proj-hlf-height* wallheight))
			 (sdl:point :x raycount :y *proj-height*) :color (sdl:color :r 95 :g 87 :b 79)
								  :surface *surface*)
	  
	  (incf rayangle *ang-increment*))))))       

(defun test1 ()
  (sdl:with-init ()
    (sdl:window *screenwidth* *screenheight* :title-caption "pissraycast" :sw t)
    (setf (sdl:frame-rate) *fps*)
    (setf *surface* (sdl:create-surface *proj-width* *proj-height*))
    (setf *player* (make-player :posx 2 :posy 2 :angle 90 :fov 60 :halffov 30 :movement 0.3 :rotation 5.0))
    (setf *ang-increment* (/ (player-fov *player*) *proj-width*)) ; changed to use scale stuff
    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()   	     
	     (sdl:clear-display sdl:*black*)
	     (keyhandler)
	     (raycasting)
	     (sdl:draw-surface (sdl:zoom-surface *scale* *scale* :surface *surface*))
	     (sdl:draw-surface-at-* (sdl:zoom-surface 3.5 3 :surface (sdl-image:load-image "img/handspray.png" :color-key-at #(0 0))) (- *screenwidth* 350) (- *screenheight* 300))
	     (sdl:update-display)))))


(test1)
