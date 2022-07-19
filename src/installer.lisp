(in-package #:maxima-jupyter)

(defclass maxima-installer (jupyter:installer)
  ()
  (:default-initargs
    :class 'kernel
    :display-name "Maxima"
    :kernel-name "maxima"
    :language "maxima"
    :systems '(:alexandria :common-lisp-jupyter :iterate)
    :local-systems '(:maxima-jupyter)
    :resources (list (asdf:component-pathname (asdf:find-component :maxima-jupyter '("res" "logo-64x64.png"))))))

(defclass user-installer (jupyter:user-installer maxima-installer)
  ())

(defclass user-image-installer (jupyter:user-image-installer maxima-installer)
  ())

(defclass system-installer (jupyter:system-bundle-installer maxima-installer)
  ())

(defmethod jupyter:command-line ((instance user-installer))
  (list
    (jupyter:installer-implementation instance)
    "--very-quiet"
    (format nil "--preload-lisp=~A"
	    (merge-pathnames 
	     (make-pathname :directory '(:relative "maxima-jupyter")
                       :name "load-maxima-jupyter"
                       :type "lisp")
	     (jupyter:installer-path instance :local-projects)))
    "--batch-string=jupyter_kernel_start(\"{connection_file}\")$"))

(defmethod jupyter:command-line :before ((instance user-image-installer))
	   (ensure-directories-exist (jupyter:installer-path instance :program)))

(defmethod jupyter:command-line ((instance system-installer))
  (let ((prefix (jupyter:installer-prefix instance)))
    (setf (jupyter:installer-prefix instance) nil)
    (unwind-protect
	 (list
	  (jupyter:installer-implementation instance)
	  "--very-quiet"
	  (format nil "--preload-lisp=~A"
		  (jupyter:installer-path instance :bundle))
	  (format nil "--preload-lisp=~A"
		  (merge-pathnames 
		   (make-pathname :directory '(:relative "maxima-jupyter")
				  :name "load-maxima-jupyter"
				  :type "lisp")
		   (jupyter:installer-path instance :local-projects)))
	  "--batch-string=jupyter_kernel_start(\"{connection_file}\")$")
      (setf (jupyter:installer-prefix instance) prefix))))
