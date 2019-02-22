
(cl-defstruct vec3 x y z)

(defun vec3-norm-sq (v)
  (+ (expt (vec3-x v) 2)
     (expt (vec3-y v) 2)
     (expt (vec3-z v) 2)))

(defun vec3-norm (v)
    (sqrt (vec3-norm-sq v)))

(defun vec3-dot (v1 v2)
  (+ (* (vec3-x v1) (vec3-x v2))
     (* (vec3-y v1) (vec3-y v2))
     (* (vec3-z v1) (vec3-z v2))))

(defun vec3-cross (v1 v2)
  (make-vec3 :x (- (* (vec3-y v1) (vec3-z v2))
                      (* (vec3-z v1) (vec3-y v2)))
                :y (- (* (vec3-z v1) (vec3-x v2))
                      (* (vec3-x v1) (vec3-z v2)))
                :z (- (* (vec3-x v1) (vec3-y v2))
                      (* (vec3-y v1) (vec3-x v2)))))

(defun vec3-add (v1 &rest vs)
  (make-vec3 :x (cl-reduce (lambda (acc v) (+ (vec3-x v) acc)) vs :initial-value (vec3-x v1))
                :y (cl-reduce (lambda (acc v) (+ (vec3-y v) acc)) vs :initial-value (vec3-y v1))
                :z (cl-reduce (lambda (acc v) (+ (vec3-z v) acc)) vs :initial-value (vec3-z v1))))

(defun vec3-sub (v1 &rest vs)
  (make-vec3 :x (cl-reduce (lambda (acc v) (- (vec3-x v) acc)) vs :initial-value (vec3-x v1))
                :y (cl-reduce (lambda (acc v) (- (vec3-y v) acc)) vs :initial-value (vec3-y v1))
                :z (cl-reduce (lambda (acc v) (- (vec3-z v) acc)) vs :initial-value (vec3-z v1))))

(defun vec3-scale (v s)
  (make-vec3 :x (* (vec3-x v) s)
                :y (* (vec3-y v) s)
                :z (* (vec3-z v) s)))

(defun vec3-zero ()
  (make-vec3 :x 0 :y 0 :z 0))

(defun vec3-forward ()
  (make-vec3 :x 1 :y 0 :z 0))

(defun vec3-left ()
  (make-vec3 :x 0 :y 1 :z 0))

(defun vec3-up ()
  (make-vec3 :x 0 :y 0 :z 1))

(ert-deftest vector-test-dot ()
  "testing dot product"
  (let ((v1 (make-vec3 :x 1 :y 2 :z 3))
        (v2 (make-vec3 :x 4 :y 5 :z 6)))
    (should (= (vec3-dot v1 v2)
               32))
    (should (= (vec3-dot (vec3-forward) (vec3-forward))
               1))
    (should (= (vec3-dot (vec3-forward) (vec3-up))
               0))))

(provide 'vec3)

