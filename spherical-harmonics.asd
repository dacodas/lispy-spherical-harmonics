;;;; spherical-harmonics.asd

(asdf:defsystem spherical-harmonics
  :description "Exploring spherical harmonics with OpenGL"
  :serial t
  :components ((:file "package")
               (:file "matrices")
               (:file "common-utilities")
               (:file "utilities")
               (:file "spherical-harmonics")
               (:file "interactive"))
  :depends-on (:cepl :cl-opengl :cl-glfw3 :png-read :cl-ppcre :matlisp))
