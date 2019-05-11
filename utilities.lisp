(in-package :spherical-harmonics)

(defparameter *binary-directory*
  (merge-pathnames
   #P"c/bin/"
   (asdf:system-source-directory :spherical-harmonics)))

(defparameter *shaders-directory*
  (merge-pathnames
   #P"shaders/"
   (asdf:system-source-directory :spherical-harmonics)))

(defmacro loop-over-phi-thetas (number-of-steps &rest body)
  (let ((steps (gensym "STEPS-")))
    `(let ((,steps ,number-of-steps))
       (loop
          for theta = 0 then (incf theta (/ pi ,steps))
          for theta-step from 0 below ,steps
          do
            (loop
               for phi = 0 then (incf phi (/ (* 2 pi) ,steps))
               for phi-step from 0 below ,steps
               do
                 ,@body)))))

(defmacro loop-over-phi-thetas-for-pgm-for-texture (number-of-steps &rest body)
  (let ((steps (gensym "STEPS-")))
    `(let ((,steps ,number-of-steps))
       (loop
          for phi = pi then (decf phi (/ (* 2 pi) ,steps))
          for phi-step from 0 below ,steps
          do
            (loop
               for theta = 0 then (decf theta (/ pi ,steps))
               for theta-step from 0 below ,steps
               do
                 ,@body)))))

(defun generate-xyz-conversion-samplers (steps)
  (symbol-macrolet ((new-array (make-array (list steps steps) :element-type 'float :initial-element 0.0)))
    (let ((x-sampler new-array)
          (y-sampler new-array)
          (z-sampler new-array))

      (loop-over-phi-thetas steps
           (let* ((float-values (list (* (sin theta) (cos phi))
                                      (* (sin theta) (sin phi))
                                      (cos theta))))
             (destructuring-bind (x y z)
                 float-values
               (setf (aref x-sampler theta-step phi-step) x)
               (setf (aref y-sampler theta-step phi-step) y)
               (setf (aref z-sampler theta-step phi-step) z))))

      (list x-sampler y-sampler z-sampler))))

(defun generate-rho-sampler (l m steps)
  (with-open-file (file (format nil "/tmp/~A-~A-~A.pgm" l m steps)
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (format file "P2 ~A ~A 255~%" steps steps)
    (symbol-macrolet ((new-string (make-array 0
                                              :element-type 'base-char
                                              :initial-element #\Null
                                              :adjustable t
                                              :fill-pointer 0)))
      (let ((input-string new-string)
            (output-string new-string))
        (with-output-to-string (input input-string)
          (format input "~A ~A~%" l m)
          (loop-over-phi-thetas-for-pgm-for-texture steps
               (format input "~f ~f~%" theta phi)))

        (with-output-to-string (output output-string)
          (sb-ext:run-program (merge-pathnames "spherical-harmonics"
                                               *binary-directory*)
                              '()
                              :input (make-string-input-stream input-string)
                              :output output))

        (let ((rho-output (make-string-input-stream output-string))
              (rho-sampler (make-array (expt steps 2)
                                       :element-type 'float
                                       :initial-element 0.0)))
          (loop
             for line = (read-line rho-output nil 'eof)
             for i = 0 then (incf i)
             while (not (eq line 'eof))
             do (progn
                  (setf (aref rho-sampler i) (/ (+ 1 (read-from-string line)) 2))
                  (format file "~A~%" (floor (* 255  (/ (+ 1 (read-from-string line)) 2))) )))
          rho-sampler)))))


