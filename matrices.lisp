(in-package :spherical-harmonics)

(defun make-matlisp-zero-matrix ()
  (make-instance
   (matlisp:tensor 'single-float 'matlisp:simple-dense-tensor)
   :dimensions (make-array 2
                           :element-type 'fixnum
                           :initial-contents '(4 4))
   :store (make-array 16
               :element-type 'single-float
               :initial-element 0.0)))

(defun make-matlisp-matrix-displaced (matrix)
  (make-instance
   (matlisp:tensor 'single-float 'matlisp:simple-dense-tensor)
   :dimensions (make-array 2
                           :element-type 'fixnum
                           :initial-contents '(4 4))
   :store (copy-seq
           (make-array 16
                       :element-type 'single-float
                       :displaced-to matrix))))

(defmacro make-matlisp-matrix (initial-contents-sexp)
  `(matlisp:transpose
    (make-instance
     (matlisp:tensor 'single-float 'matlisp:simple-dense-tensor)
     :dimensions (make-array 2
                             :element-type 'fixnum
                             :initial-contents '(4 4))
     :store (make-array 16
                        :element-type 'single-float
                        :initial-contents ,initial-contents-sexp))))

(defun make-matlisp-vector (elements)
  (matlisp:transpose
   (make-instance
    (matlisp:tensor 'single-float 'matlisp:simple-dense-tensor)
    :dimensions (make-array 1 
                            :element-type 'fixnum
                            :initial-contents '(4))
    :store (make-array 4
                       :element-type 'single-float
                       :initial-contents elements))))

(defun solve-linear-system (matrix vector)
(matlisp:getrs! 
 (matlisp:getrf!
  matrix)
 vector))

(defun matrix-multiply (matrix-1 matrix-2)
  (matlisp:gem 1 matrix-1 matrix-2 0 (make-matlisp-zero-matrix)))
