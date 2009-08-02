;; http://www.ffconsultancy.com/languages/ray_tracer/benchmark.html
;; http://www.ffconsultancy.com/languages/ray_tracer/code/1/ray.lisp

;; (main 9 "image.pgm" 512)
;; 718b12aa77a7d2fd9e038f3edef81487  image.pgm

(defconstant infinity most-positive-double-float)
(defconstant delta (sqrt double-float-epsilon))

(defstruct (vec (:conc-name nil)
		(:constructor vec (x y z))
		(:type (vector double-float)))
  x y z)

(defvar zero (vec 0d0 0d0 0d0))

(defun *v (s a) (vec (* s (x a)) (* s (y a)) (* s (z a))))
(defun +v (a b) (vec (+ (x a) (x b)) (+ (y a) (y b)) (+ (z a) (z b))))
(defun -v (a b) (vec (- (x a) (x b)) (- (y a) (y b)) (- (z a) (z b))))
(defun dot (a b) (+ (* (x a) (x b)) (* (y a) (y b)) (* (z a) (z b))))
(defun magnitude (a) (sqrt (the (double-float 0d0) (dot a a))))
(defun unitise (a) (*v (/ 1d0 (magnitude a)) a))

(defstruct ray (orig zero) (dir zero))
(defstruct sphere (center zero) (radius 0d0 :type double-float))
(defstruct (group (:include sphere)) (children nil :type list))

(defun ray-sphere (orig dir center radius)
  (let* ((v (-v center orig))
	 (b (dot v dir))
	 (disc (+ (- (* b b) (dot v v)) (* radius radius))))
    (if (< disc 0d0)
        infinity
        (let* ((disc (sqrt disc))
               (t2 (+ b disc))
               (t1 (- b disc)))
          (cond ((< t2 0d0) infinity)
                ((> t1 0d0) t1)
                (t t2))))))

(defun intersect (orig dir scene)
  (labels ((aux (lam normal scene)
             (let* ((center (sphere-center scene))
                    (lamt (ray-sphere orig
                                      dir
                                      center
                                      (sphere-radius scene))))
               (if (>= lamt lam)
                   (values lam normal)
                   (etypecase scene
		     (group
		      (dolist (kid (group-children scene))
			(setf (values lam normal)
			      (aux lam normal kid)))
		      (values lam normal))
		     (sphere
		      (values lamt (unitise
				    (-v (+v orig (*v lamt dir)) center)))))))))
    (aux infinity zero scene)))

(defun ray-trace (neg-light orig dir scene)
  (multiple-value-bind (lam normal)
      (intersect orig dir scene)
    (if (= lam infinity)
        0d0
        (let ((g (dot normal neg-light)))
          (if (< g 0d0)
              0d0
              (let ((p (+v (+v orig (*v lam dir))
                           (*v delta normal))))
		(multiple-value-bind (slam snormal)
				     (intersect p neg-light scene)
				     (if (< slam infinity)
					 0d0
				       g))))))))

(defun create (n c r)
  (let ((obj (make-sphere :center c :radius r)))
    (if (= n 1) obj
      (let ((rt (* 3d0 (/ r (sqrt 12d0)))))
	(labels ((kid (x z) (create (1- n) (+v c (vec x rt z)) (/ r 2d0))))
		(make-group :center c
			    :radius (* 3d0 r)
			    :children (list obj
					    (kid (- rt) (- rt)) (kid (- rt) rt)
					    (kid rt (- rt)) (kid rt rt))))))))

(defun main (level file-name n)
  (let* ((scene (create level (vec 0d0 -1d0 4d0) 1d0))
         (ss 4d0)
         (neg-light (unitise (vec 1d0 3d0 -2d0)))
         (-n/2 (/ n -2d0))
         (1-n/2 (1- (/ n 2d0))))
    (with-open-file (s file-name
                       :element-type '(unsigned-byte 8)
                       :if-exists :supersede
                       :if-does-not-exist :create
                       :direction :output)
      (loop for c across (format nil "P5~%~A ~A~%255~%" n n) do
            (write-byte (char-code c) s))
      (loop for y from 1-n/2 downto -n/2 do
            (loop for x from -n/2 to 1-n/2
                  for g of-type double-float = 0d0 do
                  (loop for dx from x below (1+ x) by (/ ss) do
                        (loop for dy from y below (1+ y) by (/ ss)
                              for dir = (unitise (vec dx dy (float n 0d0))) do
                              (incf g (ray-trace neg-light zero dir scene))))
                  (write-byte (round (* 255d0 (/ g (* ss ss)))) s))))))
