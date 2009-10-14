;; http://www.ffconsultancy.com/languages/ray_tracer/benchmark.html
;; http://www.ffconsultancy.com/languages/ray_tracer/code/5/ray.lisp

;; (main 9 "image.pgm" 512)
;; 718b12aa77a7d2fd9e038f3edef81487  image.pgm

(declaim (inline vec intersect sintersect ray-trace ray-sphere sray-sphere))

(defconstant infinity most-positive-double-float)
(defconstant delta (sqrt double-float-epsilon))

(defstruct (vec (:conc-name nil)
		(:constructor vec (x y z))
		(:type (vector double-float)))
  x y z)

(defmacro mvec (form)
  `(multiple-value-bind (x y z) ,form
    (vec x y z)))

(defmacro unvec (form)
  (let ((var (gensym)))
    `(let ((,var ,form))
       (values (x ,var) (y ,var) (z ,var)))))

(defvar zero (vec 0d0 0d0 0d0))

(defmacro def ((name params &body body)
               (mname &rest mparams)
               (wname &rest wparams))
  `(progn
    (declaim (inline ,name ,wname))
    (defun ,name ,params
      (declare (type double-float ,@params))
      ,@body)
    (defmacro ,mname ,(mapcar #'car mparams)
      ,(loop with inner = (list name)
             with body = ``,',inner
             with all-names = nil
             for (form count) in (reverse mparams)
             for names = (loop repeat count collect (gensym))
             do
             (setf all-names (append all-names names))
             (setf body ``(multiple-value-bind ,',(reverse names)
                           ,,form ,,body))
             finally
             (setf (cdr inner) (reverse all-names))
             (return body)))
    (defun ,wname ,(mapcar #'car wparams)
      (,mname ,@(mapcar #'cadr wparams)))))

(def (v*/values (s rx ry rz)
       (values (* s rx)
               (* s ry)
               (* s rz)))
     (mv* (s 1) (form 3))
     (v* (s s) (r (unvec r))))

(def (dot/values (ax ay az bx by bz)
       (+ (* ax bx)
          (* ay by)
          (* az bz)))
     (mdot (a 3) (b 3))
     (dot (a (unvec a)) (b (unvec b))))

(def (unitise/values (rx ry rz)
       (v*/values (/ 1 (sqrt (abs (dot/values rx ry rz rx ry rz))))
                  rx ry rz))
     (munitise (form 3))
     (unitise (r (unvec r))))

(def (magnitude/values (rx ry rz)
       (sqrt (abs (dot/values rx ry rz rx ry rz))))
     (mmagnitude (form 3))
     (magnitude (r (unvec r))))

(defmacro defvfun (name mname wname op)
  `(def (,name (ax ay az bx by bz)
         (values (,op ax bx)
          (,op ay by)
          (,op az bz)))
    (,mname (a 3) (b 3))
    (,wname (a (unvec a)) (b (unvec b)))))

(defvfun v+/values mv+ v+ +)
(defvfun v-/values mv- v- -)

(defstruct ray
  (orig zero)
  (dir zero))

(defstruct sphere
  (center zero)
  (radius 0d0 :type double-float))

(defstruct (group (:include sphere))
  (children nil :type list))

(defun ray-sphere (orig dir center radius)
  (multiple-value-bind (x y z)
      (v- center orig)
    (let* ((b (dot/values x y z
                          (x dir) (y dir) (z dir)))
           (disc (+ (- (* b b)
                       (dot/values x y z
                                   x y z))
                    (expt radius 2))))
      (if (< disc 0d0)
          infinity
          (let* ((disc (sqrt disc))
                 (t2 (+ b disc))
                 (t1 (- b disc)))
            (cond ((< t2 0d0) infinity)
                  ((> t1 0d0) t1)
                  (t t2)))))))

(defun sray-sphere (orig dir center radius)
  (multiple-value-bind (x y z)
      (v- center orig)
    (let* ((b (dot/values x y z
                          (x dir) (y dir) (z dir)))
           (disc (+ (- (* b b)
                       (dot/values x y z
                                   x y z))
                    (* radius radius))))
      (if (< disc 0d0)
          nil
          (>= (+ b (sqrt disc)) 0d0)))))

(defun intersect (orig dir scene)
  (labels ((aux (lam scene nx ny nz)
             (declare (double-float lam))
             (let* ((center (sphere-center scene))
                    (lamt (ray-sphere orig dir center
                                      (sphere-radius scene))))
               (if (>= lamt lam)
                   (values lam nx ny nz)
                   (etypecase scene
		     (group
                      (dolist (kid (group-children scene))
                        (setf (values lam nx ny nz)
                              (aux lam kid nx ny nz)))
                      (values lam nx ny nz))
		     (sphere
                      (multiple-value-bind (x y z)
                          (munitise (mv- (mv+ (unvec orig)
                                              (v* lamt dir))
                                         (unvec center)))
                        (values lamt x y z))))))))
    (aux infinity scene 0d0 0d0 0d0)))

(defun sintersect (orig dir scene)
  (labels ((aux (scene)
             (when (sray-sphere orig
                                dir
                                (sphere-center scene)
                                (sphere-radius scene))
               (etypecase scene
                 (group (some #'aux (group-children scene)))
                 (sphere t)))))
    (aux scene)))

(defun ray-trace (neg-light orig dir scene)
  (multiple-value-bind (lam x y z)
      (intersect orig dir scene)
    (if (= lam infinity)
        0d0
        (let ((g (mdot (values x y z)
                       (unvec neg-light))))
          (if (< g 0d0)
              0d0
              (let ((p (mvec (mv+ (mv+ (unvec orig) (v* lam dir))
                                  (v*/values delta x y z)))))
                (if (sintersect p neg-light scene)
                    0d0
                    g)))))))

(defun create (n c r)
  (let ((obj (make-sphere :center c :radius r)))
    (if (= n 1)
        obj
        (let ((rt (* 3d0 (/ r (sqrt 12d0)))))
          (flet ((kid (x z)
                   (create (1- n)
                           (mvec (v+ c (vec x rt z)))
                           (/ r 2d0)))
                 (bound (c scenes)
                   (loop for s in scenes
                         maximize (+ (mmagnitude (v- c (sphere-center s)))
                                     (sphere-radius s)))))
            (let ((objs (list obj
                              (kid (- rt) (- rt))
                              (kid (- rt) rt)
                              (kid rt (- rt))
                              (kid rt rt)))
                  (center (vec (x c) (+ (y c) r) (z c))))
              (make-group :center center
                          :radius (max (bound center objs) r)
                          :children objs)))))))

(defun main (level file-name n)
  (declare (fixnum level n)
           (optimize speed (safety 0)))
  (let* ((scene (create level (vec 0d0 -1d0 4d0) 1d0))
         (ss 4d0)
         (neg-light (mvec (unitise (vec 1d0 3d0 -2d0))))
         (ray (make-ray))
         (-n/2 (/ n -2.0d0))
         (1-n/2 (1- (/ n 2.0d0))))
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
                              for dir = (ray-dir ray) do
                              (setf (values (x dir) (y dir) (z dir))
                                    (unitise/values dx dy (float n 0d0)))
                              (incf g (ray-trace neg-light zero dir scene))))
                  (write-byte (round (* 255.0d0 (/ g (* ss ss)))) s))))))
