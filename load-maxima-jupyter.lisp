(maxima::$load "mactex-utilities")
(maxima::$load "stringproc")

(setq maxima::$linenum 0)
(setq maxima::*display-labels-p* t)

(pushnew (make-pathname :device (pathname-device *load-truename*)
                        :directory (pathname-directory *load-truename*))
         ql:*local-project-directories*)

(ql:register-local-projects)

(ql:quickload :maxima-jupyter)

; (declaim (optimize (speed 0) (space 0) (debug 3) (safety 3)))
