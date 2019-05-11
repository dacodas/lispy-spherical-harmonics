(in-package :spherical-harmonics)

(defun update-rho-map ()
  (sb-thread:interrupt-thread
   (find-if (lambda (el) (equal (sb-thread:thread-name el) "spherical-harmonics")) (sb-thread:list-all-threads))
   (lambda ()
     (defparameter *picture-rho*
       (make-array '(128 128)
			       :element-type 'float
			       :displaced-to (generate-rho-sampler 50 128)
			       :displaced-index-offset 0))
     (let ((texture *texture-rho*)
           (picture *picture-rho*))
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
					                  :displaced-index-offset 0)))))))
