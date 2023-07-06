;;;; test one - funny square

;;; lib
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :lispbuilder-sdl))

(defparameter *mapwidth* 24)
(defparameter *mapheight* 24)
(defparameter *screenwidth* 640)
(defparameter *screenheight* 480)
(defparameter *half-height* (/ *screenheight* 2))
(defparameter *half-width* (/ *screenwidth* 2))
(defparameter *fps* 30)
(defparameter *ang-increment* nil) ; Set later as it relies on player fov
(defparameter *ray-precision* 64)

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


    

(defun raycasting ()
  (let ((rayangle (- (player-angle *player*) (player-halffov *player*))))
	
    (dotimes (raycount *screenwidth*)
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
	       (wallheight (floor (/ *half-height* distance))))

	  ;;Draw the shit yass
	  (sdl:draw-line (sdl:point :x raycount :y 0)
			 (sdl:point :x raycount :y (- *half-height* wallheight)) :color sdl:*cyan*)
	  
	  (sdl:draw-line (sdl:point :x raycount :y (- *half-height* wallheight))
			 (sdl:point :x raycount :y (+ *half-height* wallheight)) :color sdl:*red*)

	  (sdl:draw-line (sdl:point :x raycount :y (+ *half-height* wallheight))
			 (sdl:point :x raycount :y *screenheight*) :color sdl:*green*)
	  
	  (incf rayangle *ang-increment*))))))       

(defun test1 ()
  (sdl:with-init ()
    (sdl:window *screenwidth* *screenheight* :title-caption "pissraycast")
    (setf *player* (make-player :posx 2 :posy 2 :angle 90 :fov 60 :halffov 30 :movement 0.3 :rotation 5.0))
    (setf *ang-increment* (/ (player-fov *player*) *screenwidth*))
    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()   	     
	     (sdl:clear-display sdl:*black*)
	     (keyhandler)
	     (raycasting)
	     (sdl:update-display)))))


(test1)
