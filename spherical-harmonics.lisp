(in-package :spherical-harmonics)

(eval-when (:compile-toplevel)
  (defparameter *shader-count* 0))

(defparameter *see-texture* nil)

(defparameter *window-width* 1920)
(defparameter *window-height* 1080)

(defparameter *surface-shader-program* -1)
(defparameter *points-shader-program* -1)
(defparameter *texture-shader-program* -1)

(defparameter *shader-time* 0)

(defparameter *resolution* 200)
(defparameter *triangle-offset* 0)
(defparameter *triangles-to-draw* (* *resolution* *resolution* 2))

(defparameter *eye-rho* 20)
(defparameter *eye-theta* (coerce (/ pi 2.0) 'single-float))
(defparameter *eye-phi* 0.01)
(defparameter *fov* (coerce (/ pi 4.0) 'single-float))

(defparameter *identity* (make-array '(4 4) :element-type 'single-float
				                     :initial-contents '((1.0 0.0 0.0 0.0)
							                             (0.0 1.0 0.0 0.0)
							                             (0.0 0.0 1.0 0.0)
							                             (0.0 0.0 0.0 1.0))))

(cl-glfw3:def-key-callback key-callback (window key scancode action mod-keys)

  (declare (ignore window scancode mod-keys))

  (when (and (equal key :x) (eq action :press))
    (setup-surface-shader-program)
    (setup-points-shader-program)
    (setup-texture-shader-program))
  (when (and (equal key :t) (eq action :press))
    (setf *see-texture* (not *see-texture*)))
  (when (and (equal key :d) (eq action :press))
      (incf *eye-phi* 0.1))
  (when (and (equal key :a) (eq action :press))
      (decf *eye-phi* 0.1))
  (when (and (equal key :w) (eq action :press))
      (decf *eye-theta* 0.1))
  (when (and (equal key :s) (eq action :press))
      (incf *eye-theta* 0.1))
  (when (and (equal key :up) (eq action :press))
      (decf *eye-rho* 0.5))
  (when (and (equal key :down) (eq action :press))
      (incf *eye-rho* 0.5))
  (when (and (eq key :escape) (eq action :press))
    (cl-glfw3:set-window-should-close))
  (when (and (eq key :q) (eq action :press))
    (format t "Eye position: ~a~%" (list (* *eye-rho* (sin *eye-theta*) (cos *eye-phi*))
			                             (* *eye-rho* (sin *eye-theta*) (sin *eye-phi*))
			                             (* *eye-rho* (cos *eye-theta*))))))

(defparameter *model-matrix* *identity*)
(defparameter *view-matrix* *identity*)
(defparameter *projection-matrix* *identity*)

(defun matrix-4v-to-simple-array (matrix)
  (make-array 16 :element-type 'single-float :initial-contents
              (make-array 16 :element-type 'single-float :displaced-to matrix)))

(defun make-shader-program (vertex-source fragment-source)
  ;; Keep trying to load our shader (Allow user to fix compile errors)
  
  (destructuring-bind (vert-shader frag-shader shader-program) 
      (macrolet ((make-shaders ()
                   (let ((vert-shader-symbol (intern (format nil "*VERT-SHADER-~A*" *shader-count*)))
                         (frag-shader-symbol (intern (format nil "*FRAG-SHADER-~A*" *shader-count*)))
                         (shader-program-symbol (intern (format nil "*SHADER-PROGRAM-~A*" *shader-count*))))
                     (incf *shader-count*)
                     `(progn
                        (defparameter ,vert-shader-symbol (gl:create-shader :vertex-shader))
                        (defparameter ,frag-shader-symbol (gl:create-shader :fragment-shader))
                        (defparameter ,shader-program-symbol (gl:create-program))

                        (list ,vert-shader-symbol ,frag-shader-symbol ,shader-program-symbol)))))
        (make-shaders))

    ;; Copy our shader source to the OpenGL shader
    (gl:shader-source vert-shader vertex-source)
    (gl:shader-source frag-shader fragment-source)

    ;; Compile our shader sources into GPU bytecode
    (gl:compile-shader vert-shader)
    (gl:compile-shader frag-shader)

    (check-shader-error vert-shader)
    (check-shader-error frag-shader)

    ;; Then add our shaders to that program
    ;; The same shader can be attached to different programs
    (gl:attach-shader shader-program vert-shader)
    (gl:attach-shader shader-program frag-shader)

    (gl:link-program shader-program)

    shader-program))

(defun render ()
  (gl:clear-depth 1.0)
  (gl:clear :color-buffer)
  (gl:clear :depth-buffer)

  (setf *view-matrix*
	    (let ((eye (list (* *eye-rho* (sin *eye-theta*) (cos *eye-phi*))
			             (* *eye-rho* (sin *eye-theta*) (sin *eye-phi*))
			             (* *eye-rho* (cos *eye-theta*)))))
	      (look-at (make-array 3 :initial-contents eye)
		           (make-array 3 :initial-contents (list 0.0 0.0 0.0))
		           (make-array 3 :initial-contents (list 0.0 0.0 1.0)))))

  (setf *projection-matrix* (perspective *fov* (/ *window-width* *window-height*)  0.1 10000))

  

  

  (let* ((max-amplitude 0.5))
    (loop for shader-program in (list *surface-shader-program*
                                      *points-shader-program*)
       do (progn
            (gl:use-program shader-program)

            (gl:uniform-matrix-4fv
             (gl:get-uniform-location shader-program "model")
             (matrix-4v-to-simple-array *model-matrix*))
            
            (gl:uniform-matrix-4fv
             (gl:get-uniform-location shader-program "view")
             (matrix-4v-to-simple-array *view-matrix*)
             nil)

            (gl:uniform-matrix-4fv
             (gl:get-uniform-location shader-program "proj")
             (matrix-4v-to-simple-array *projection-matrix*)
             nil)

            
            (let ((current-amplitude (* max-amplitude (cos (* *shader-time*)))))
              (gl:uniformf (gl:get-uniform-location *surface-shader-program* "amplitude")
                           current-amplitude))))

    (gl:use-program *surface-shader-program*)
    (gl:uniformf (gl:get-uniform-location *surface-shader-program* "max_amplitude")
                 max-amplitude))


  (incf *shader-time* .1)

  (gl:active-texture 0)
  (gl:uniformi (gl:get-uniform-location *surface-shader-program* "x_sampler") 0)
  (gl:bind-texture :texture-2d *texture-x*)

  (gl:active-texture 1)
  (gl:uniformi (gl:get-uniform-location *surface-shader-program* "y_sampler") 1)
  (gl:bind-texture :texture-2d *texture-y*)

  (gl:active-texture 2)
  (gl:uniformi (gl:get-uniform-location *surface-shader-program* "z_sampler") 2)
  (gl:bind-texture :texture-2d *texture-z*)

  (gl:active-texture 3)
  (gl:uniformi (gl:get-uniform-location *surface-shader-program* "rho_sampler") 3)
  (gl:bind-texture :texture-2d *texture-rho*)

  (gl:uniformi (gl:get-uniform-location *surface-shader-program* "rho_sampler_vertex") 3)
  (gl:bind-texture :texture-2d *texture-rho*)

  (gl:use-program *points-shader-program*)

  (gl:active-texture 3)
  (gl:uniformi (gl:get-uniform-location *points-shader-program* "rho_sampler_vertex") 3)
  (gl:bind-texture :texture-2d *texture-rho*)

  (gl:use-program *texture-shader-program*)
  (gl:uniformf (gl:get-uniform-location *texture-shader-program* "screen_scale")
               (/ *window-width* *window-height*))

  (gl:active-texture 3)
  (gl:uniformi (gl:get-uniform-location *texture-shader-program* "rho_sampler") 3)
  (gl:bind-texture :texture-2d *texture-rho*)

  
  (progn
    (gl:bind-buffer :array-buffer *surface-vertex-buffer*)
    (gl:bind-buffer :element-array-buffer *surface-element-buffer*)

    (gl:use-program *points-shader-program*)

    (gl:vertex-attrib-pointer (gl:get-attrib-location *points-shader-program* "vertex_position") 2 :float nil 0 (cffi:null-pointer))

    (gl:point-size 1.0)
    #+nil(gl:draw-arrays :points
                    *triangle-offset*
                    *triangles-to-draw*)

    (gl:use-program *surface-shader-program*)


    (gl:draw-elements :triangle-strip
                      (let ((array (opengl:make-null-gl-array :unsigned-short)))
                        (setf (opengl::gl-array-pointer array)
                              (cffi:make-pointer *triangle-offset*))
                        array)
                      :count *triangles-to-draw*)

    

    
    
    
    (let* ((start -10.0)
           (end 10.0)
           (inc 5.0)
           (x-angle (* 0.1 *shader-time*))
           (y-angle (* 0.1 *shader-time*))
           (z-angle (* 0.5 *shader-time*))
           (rotation-matrix
            (mmult
             (mmult
              (make-array '(4 4) :element-type 'single-float
				          :initial-contents `((1.0 0.0 0.0 0.0)
							                  (0.0 ,(cos x-angle) ,(- (sin x-angle)) 0.0)
							                  (0.0 ,(sin x-angle) ,(cos x-angle) 0.0)
							                  (0.0 0.0 0.0 1.0)))
              (make-array '(4 4) :element-type 'single-float
				          :initial-contents `((,(cos y-angle) 0.0 ,(sin y-angle) 0.0)
                                              (0.0 1.0 0.0 0.0)
                                              (,(- (sin y-angle)) 0.0 ,(cos y-angle) 0.0)
                                              (0.0 0.0 0.0 1.0))))
             (make-array '(4 4) :element-type 'single-float
				         :initial-contents
                         `((,(cos z-angle) ,(- (sin z-angle)) 0.0 0.0)
                           (,(sin z-angle) ,(cos z-angle) 0.0 0.0) 
                           (0.0 0.0 1.0 0.0)
                           (0.0 0.0 0.0 1.0))))))
      (loop for x = start then (incf x inc)
         while (<= x end)
         do (loop for y = start then (incf y inc)
               while (<= y end)
               do
                 (loop for z = start then (incf z inc)
                    while (<= z end)

                    do (let* ((translation-matrix
                               (make-array '(4 4) :element-type 'single-float
				                           :initial-contents `((1.0 0.0 0.0 ,x)
							                                   (0.0 1.0 0.0 ,y)
							                                   (0.0 0.0 1.0 ,z)
							                                   (0.0 0.0 0.0 1.0))))
                              (model-matrix
                               (mmult translation-matrix
                                      rotation-matrix)))

                         (gl:use-program *points-shader-program*)

                         (gl:uniform-matrix-4fv
                          (gl:get-uniform-location *points-shader-program* "model")
                          (matrix-4v-to-simple-array model-matrix))

                         #+nil(gl:draw-arrays :points
                                         *triangle-offset*
                                         *triangles-to-draw*)

                         (gl:use-program *surface-shader-program*)
                         (gl:uniform-matrix-4fv
                          (gl:get-uniform-location *surface-shader-program* "model")
                          (matrix-4v-to-simple-array model-matrix))
                         (gl:draw-elements :triangle-strip
                                           (let ((array (opengl:make-null-gl-array :unsigned-short)))
                                             (setf (opengl::gl-array-pointer array)
                                                   (cffi:make-pointer *triangle-offset*))
                                             array)
                                           :count *triangles-to-draw*)))))))

  (progn
    (gl:use-program *texture-shader-program*)

    (gl:bind-buffer :array-buffer *texture-vertex-buffer*)
    (gl:bind-buffer :element-array-buffer *texture-element-buffer*)

    (gl:vertex-attrib-pointer (gl:get-attrib-location *texture-shader-program* "vertex_position") 2 :float nil 0 (cffi:null-pointer))

    (gl:draw-elements :triangle-strip
                      (opengl:make-null-gl-array :unsigned-short)
                      :count 4)))

(defun read-sphere-texture (pathname)
  (let ((image-data (png-read:image-data (png-read:read-png-file pathname))))
    (destructuring-bind (rows columns)
        (array-dimensions image-data)
      (let ((new-array (make-array (list rows columns 4) :initial-element 0 :element-type '(unsigned-byte 8))))
        (loop for row below rows
           do (loop for column below columns 
                 do (let ((original-value (aref image-data row column)))
                      (setf (aref new-array row column 0) original-value
                            (aref new-array row column 1) original-value
                            (aref new-array row column 2) original-value
                            (aref new-array row column 3) 255))))
        new-array))))



(defun setup-points-shader-program ()
  (setf *points-shader-program*
        (make-shader-program
         (read-shader (merge-pathnames #P"points.vertex.shader"
                                       *shaders-directory*))
         (read-shader (merge-pathnames #P"points.fragment.shader"
                                       *shaders-directory*)))))

(defun setup-surface-shader-program ()
  (setf *surface-shader-program*
        (make-shader-program
         (read-shader (merge-pathnames #P"surface.vertex.shader"
                                       *shaders-directory*))
         (read-shader (merge-pathnames #P"surface.fragment.shader"
                                       *shaders-directory*)))))

(defun setup-texture-shader-program ()
  (setf *texture-shader-program*
        (make-shader-program
         (read-shader (merge-pathnames #P"texture.vertex.shader"
                                       *shaders-directory*))
         (read-shader (merge-pathnames #P"texture.fragment.shader"
                                       *shaders-directory*)))))

(defun run ()
  (cl-glfw3:with-init-window (:title "Window test" :width 1920 :height 1080) 
    (setf %gl:*gl-get-proc-address* #'cl-glfw3:get-proc-address)
    ;; (cl-glfw3:set-key-callback 'quit-on-escape)
    (cl-glfw3:set-key-callback 'key-callback)
    (cl-glfw3:set-window-size-callback 'update-viewport)

    (setup-surface-shader-program)
    (setup-points-shader-program)
    (setup-texture-shader-program)

    (destructuring-bind (x y z)
        (generate-xyz-conversion-samplers 128)
      (defparameter *picture-x* x)
      (defparameter *picture-y* y)
      (defparameter *picture-z* z))

    (defparameter *picture-rho*
      (make-array '(128 128)
			      :element-type 'float
			      :displaced-to (generate-rho-sampler 3 0 128)
			      :displaced-index-offset 0))

    (defparameter *texture-x* (first (gl:gen-textures 1)))
    (defparameter *texture-y* (first (gl:gen-textures 1)))
    (defparameter *texture-z* (first (gl:gen-textures 1)))
    (defparameter *texture-rho* (first (gl:gen-textures 1)))



    ;; (let ((symbols '(x y z rho)))
    ;;   (mapcar (lambda (texture-symbol texture-number)
    ;;             (macrolet ((hooey (ts tn)
    ;;                          `(eval-when (:execute) (defparameter ,ts ,tn))))
    ;;               (hooey texture-symbol texture-number)))

    ;;           (loop for symbol in symbols 
    ;;              collect (intern (format nil "*TEXTURE-~A*" (symbol-name symbol))
    ;;                              :spherical-harmonics))

    ;;           (gl:gen-textures (length symbols))))

    ;; (break)

    (gl:enable :blend)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (gl:enable :depth-test)
    (gl:clear-color .2 .2 .2 1.0)
    (gl:disable :cull-face)

    (mapcar (lambda (texture picture)
	          (let ((dimensions (array-dimensions picture)))
	            (gl:bind-texture :texture-2d texture)
	            (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
	            (gl:tex-parameter :texture-2d :texture-min-filter :linear)
	            (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
	            (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
	            (gl:tex-image-2d :texture-2d 0 :red (second dimensions) (first dimensions) 0 :red :float
			                     (make-array (apply #'* dimensions)
					                         :element-type 'float
					                         :displaced-to picture
					                         :displaced-index-offset 0))))
	        (list *texture-x* *texture-y* *texture-z* *texture-rho*)
	        (list *picture-x* *picture-y* *picture-z* *picture-rho*))
    

    (let ((steps *resolution*))
      (destructuring-bind (coordinates elements)
          (let ((coordinates (make-array (list (1+ (* steps (1+ steps))) 2) :element-type 'float)))
            (loop
               for theta-step upto steps
               for theta = 0.0 then (incf theta (/ 1.0 steps))
               do (loop
                     for phi-step upto steps
                     for phi = 0.0 then (incf phi (/ 1.0 steps))
                     do (setf (aref coordinates (+ (* steps theta-step) phi-step) 0) theta
                              (aref coordinates (+ (* steps theta-step) phi-step) 1) phi)))
            (let ((elements (make-array (* steps steps 2) :element-type 'float))
                  (counter -1))
              (loop for step below steps
                 do (loop for stepito below steps
                       do (setf (aref elements (incf counter)) (+ (* steps step) stepito)
                                (aref elements (incf counter)) (+ (* steps (+ step 1)) stepito))))
              (list coordinates elements)))

        (defparameter *coordinates* coordinates)
        (defparameter *elements* elements)

        (defparameter *surface-vertex-buffer* (elt (gl:gen-buffers 1) 0))
        (defparameter *surface-element-buffer* (elt (gl:gen-buffers 1) 0))

        (gl:bind-buffer :array-buffer *surface-vertex-buffer*)
        (let ((arr (gl:alloc-gl-array :float (* steps steps 2)))
              (verts (make-array (* steps steps 2) :element-type 'float :displaced-to coordinates :displaced-index-offset 0)))
          (dotimes (i (length verts))
            (setf (gl:glaref arr i) (aref verts i)))
          (gl:buffer-data :array-buffer :static-draw arr)
          (gl:free-gl-array arr)

          (gl:bind-buffer :element-array-buffer *surface-element-buffer*)
          (let ((arr (gl:alloc-gl-array :unsigned-short (* steps steps 2)))
                (verts elements))
            (dotimes (i (length verts))
              (setf (gl:glaref arr i) (aref verts i)))
            (gl:buffer-data :element-array-buffer :static-draw arr)
            (gl:free-gl-array arr))) 

        (gl:enable-vertex-attrib-array 0)
        (gl:vertex-attrib-pointer (gl:get-attrib-location *surface-shader-program* "vertex_position") 2 :float nil 0 (cffi:null-pointer))))


    (defparameter *texture-vertex-buffer* (first (gl:gen-buffers 1)))
    (defparameter *texture-element-buffer* (first (gl:gen-buffers 1)))

    (let* ((center-x -.5)
           (center-y .5)
           (width 0.1)
           (window-scale (coerce (/ *window-width* *window-height*) 'single-float))
           (points (vector 0.0 0.0
                           0.0 1.0
                           (/ 1 window-scale) 0.0
                           (/ 1 window-scale) 1.0))
           (points-gl-array (gl:alloc-gl-array :float (* 4 2))))
      (gl:bind-buffer :array-buffer *texture-vertex-buffer*)
      (dotimes (i (length points))
        (setf (gl:glaref points-gl-array i) (aref points i)))
      (gl:buffer-data :array-buffer :static-draw points-gl-array)
      (gl:free-gl-array points-gl-array)
      (gl:use-program *texture-shader-program*))


    (let ((elements #(0 1 2 3))
          (elements-gl-array (gl:alloc-gl-array :unsigned-short (* 4 2))))
      (gl:bind-buffer :element-array-buffer *texture-element-buffer*)
      (dotimes (i (length elements))
        (setf (gl:glaref elements-gl-array i) (aref elements i)))
      (gl:buffer-data :element-array-buffer :static-draw elements-gl-array)
      (gl:free-gl-array elements-gl-array))

    (gl:enable-vertex-attrib-array 0)
    

    (loop until (cl-glfw3:window-should-close-p)
       do (let* ((current-time (get-internal-run-time))
                 (target-time (+ current-time (/ 1000 60.0))))
            (render)
            (cl-glfw3:swap-buffers)
            (cl-glfw3:poll-events)
            (let ((sleep-time (/ (- target-time (get-internal-run-time)) 1000)))
              (if (> sleep-time 0)
                  (sleep sleep-time)
                  (format t "Exceeded time...~%")))))))


  

