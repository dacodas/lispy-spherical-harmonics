(in-package :spherical-harmonics)

(cl-glfw3:def-window-size-callback update-viewport (window w h)
  (declare (ignore window))

  (let* ((desired-height (* (/ *window-height* *window-width*) w))
         (height-offset (/ (- h desired-height) 2)))
    
    (gl:viewport 0 height-offset w desired-height))

  ;; (set-viewport w (* (/  *window-height* *window-width*) h))
  )

(defun set-viewport (w h)
  (gl:viewport 0 0 w h))

(defun mmult (a b)
  (loop
       with m = (array-dimension a 0)
       with n = (array-dimension a 1)
       with l = (array-dimension b 1)
       with c = (make-array (list m l) :initial-element 0.0 :element-type 'single-float)
       for i below m do
              (loop for k below l do
                    (setf (aref c i k)
                          (loop for j below n
                                sum (* (aref a i j)
                                       (aref b j k)))))
       finally (return c)))


;; TODO: Don't make a list and then convert to an array, just make an
;; empty array and then fill it with aref
(defun normalize (vector)
  (if (equalp vector #(0 0 0))
      vector
      (let ((vector-length (sqrt (loop for x across vector sum (expt x 2))))
	        (result (make-array (length vector) :element-type 'single-float :adjustable nil))) 
        
        (if (equal vector-length 0) vector
	        (progn
	          (loop for i below (length vector) 
	             do (setf (aref result i) (/ (aref vector i) vector-length))) 
	          result)))))

(defun dot (u v)
  (loop for x across u
     for y across v
     sum (* x y)))

(defun cross (u v)
  (let ((result (make-array 3 :element-type 'single-float :adjustable nil)))
    (setf (aref result 0) (- (* (aref u 1) (aref v 2)) (* (aref u 2) (aref v 1))))
    (setf (aref result 1) (- (* (aref u 2) (aref v 0)) (* (aref u 0) (aref v 2))))
    (setf (aref result 2) (- (* (aref u 0) (aref v 1)) (* (aref u 1) (aref v 0))))
    result))

(defun negative (vector)
  (let ((result (make-array 3 :element-type 'single-float :adjustable nil))) 
    (loop for i below 3
       do (setf (aref result i) (- (aref vector i))))
    result))

(defun vector-subtract (u v)
  (let ((result (make-array 3 :element-type 'single-float :adjustable nil))) 
    (loop for i below 3
       do (setf (aref result i) (- (aref u i) (aref v i))))
    result))

(defun look-at (eye center up)
  (let* ((f (normalize (vector-subtract center eye)))
         (s (normalize (cross f up)))
         (u (cross s f))
	 (result (make-array '(4 4) :element-type 'single-float :adjustable nil :initial-element 0.0)))
    (setf (aref result 0 0) (aref s 0))
    (setf (aref result 1 0) (aref s 1))
    (setf (aref result 2 0) (aref s 2))
    (setf (aref result 0 1) (aref u 0))
    (setf (aref result 1 1) (aref u 1))
    (setf (aref result 2 1) (aref u 2))
    (setf (aref result 0 2) (- (aref f 0)))
    (setf (aref result 1 2) (- (aref f 1)))
    (setf (aref result 2 2) (- (aref f 2)))
    (setf (aref result 3 0) (- (dot s eye)))
    (setf (aref result 3 1) (- (dot u eye)))
    (setf (aref result 3 2) (dot f eye))
    (setf (aref result 3 3) 1.0)
    result))

(cl-glfw3:def-key-callback quit-on-escape (window key scancode action mod-keys)
                           (declare (ignore window scancode mod-keys))
                           (when (and (eq key :escape) (eq action :press))
                             (cl-glfw3:set-window-should-close)))

(define-condition compile-error (error)
  ((message
    :initform nil
    :initarg :message
    :reader compile-error-message
    :documentation "The reason given for the error")))

(defun check-shader-error (shader)
  "Get the current error status of a shader, throw error if status"
  (let ((error-string (gl:get-shader-info-log shader)))
    (unless (equalp error-string "")
      (progn

        ;; Print to console & then throw error
        (format t "~A~%" error-string)
        (error
         'compile-error
         :message error-string)))))

(defun is-invalid-shader (shader)
  (= shader -1))

(defun perspective (fovy aspect z-near z-far)
  (let ((result (make-array '(4 4) :element-type 'single-float :adjustable nil :initial-element 0.0))
	(tan-half-fovy (tan (/ fovy 2.0))))

    ;; mat<4, 4, T, defaultp> Result(static_cast<T>(0));
    ;; Result[0][0] = static_cast<T>(1) / (aspect * tanHalfFovy);
    ;; Result[1][1] = static_cast<T>(1) / (tanHalfFovy);
    ;; Result[2][2] = - (zFar + zNear) / (zFar - zNear);
    ;; Result[2][3] = - static_cast<T>(1);
    ;; Result[3][2] = - (static_cast<T>(2) * zFar * zNear) / (zFar - zNear);

		;; assert(abs(aspect - std::numeric_limits<T>::epsilon()) > static_cast<T>(0));

		;; T const tanHalfFovy = tan(fovy / static_cast<T>(2));

		;; mat<4, 4, T, defaultp> Result(static_cast<T>(0));
		;; Result[0][0] = static_cast<T>(1) / (aspect * tanHalfFovy);
		;; Result[1][1] = static_cast<T>(1) / (tanHalfFovy);
		;; Result[2][2] = - (zFar + zNear) / (zFar - zNear);
		;; Result[2][3] = - static_cast<T>(1);
		;; Result[3][2] = - (static_cast<T>(2) * zFar * zNear) / (zFar - zNear);
		;; return Result;

    (setf (aref result 0 0) (/ 1.0 (* aspect tan-half-fovy)))
    (setf (aref result 1 1) (/ 1.0 tan-half-fovy))
    (setf (aref result 2 2) (- (/ (+ z-far z-near) (- z-far z-near))))
    (setf (aref result 2 3) -1.0)
    (setf (aref result 3 2) (- (/ (* 2.0 z-far z-near) (- z-far z-near))))

    result))

(defun read-shader (shader-pathname) 
  (with-open-file (shader shader-pathname)
    (let ((contents (make-sequence 'string (file-length shader))))
      (read-sequence contents shader)
      contents)))
