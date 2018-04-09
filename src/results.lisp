(in-package #:maxima-jupyter)

#|

Standard MIME types

|#

(defvar *html-mime-type* "text/html")
(defvar *javascript-mime-type* "application/javascript")
(defvar *jpeg-mime-type* "image/jpeg")
(defvar *json-mime-type* "application/json")
(defvar *latex-mime-type* "text/latex")
(defvar *markdown-mime-type* "text/markdown")
(defvar *plain-text-mime-type* "text/plain")
(defvar *png-mime-type* "image/png")
(defvar *svg-mime-type* "image/svg+xml")



(defun plot-p (value)
  (and (listp value)
       (eq (caar value) 'maxima::mlist)
       (eq (list-length value) 3)
       (stringp (second value))
       (stringp (third value))
       (or (ends-with-p (second value) ".gnuplot")
           (ends-with-p (second value) ".gnuplot_pipes"))))

(defun sexpr-to-text (value)
  (format nil "~S" value))

(defun mexpr-to-text (value)
  (with-output-to-string (f)
    (maxima::mgrind value f)))

(defun mexpr-to-latex (value)
  (let ((env (maxima::get-tex-environment value)))
    (apply #'concatenate 'string
           (mapcar #'string
                   (maxima::tex value
                                (list (car env)) (list (cdr env))
                                'maxima::mparen 'maxima::mparen)))))

(defgeneric display (results)
  (:documentation "Display results."))

(defmethod display (res))

(defclass result ()
  ())

(defclass sexpr-result (result)
  ((value :initarg :value
          :reader sexpr-result-value)))

(defmethod display ((res sexpr-result))
  (jsown:new-js
    (*plain-text-mime-type* (sexpr-to-text (sexpr-result-value res)))))

(defclass mexpr-result (result)
  ((value :initarg :value
          :reader mexpr-result-value)))

(defmethod display ((res mexpr-result))
  (let ((value (mexpr-result-value res)))
    (jsown:new-js
      (*plain-text-mime-type* (mexpr-to-text value))
      (*latex-mime-type* (mexpr-to-latex value)))))

(defclass inline-result (result)
  ((value :initarg :value
          :reader inline-result-value)
   (mime-type :initarg :mime-type
              :reader inline-result-mime-type)))

(defun make-inline-result (value &key (mime-type *plain-text-mime-type*))
 (make-instance 'inline-result :value value
                               :mime-type mime-type))

(defmethod display ((res inline-result))
  (let ((value (inline-result-value res))
        (mime-type (inline-result-mime-type res)))
    (if (equal mime-type *plain-text-mime-type*)
      (jsown:new-js
        (mime-type value))
      (jsown:new-js
        (*plain-text-mime-type* "inline-value")
        (mime-type (if (stringp value)
                       value
                       (cl-base64:usb8-array-to-base64-string value)))))))

(defclass file-result (result)
  ((path :initarg :path
         :reader file-result-path)
   (mime-type :initarg :mime-type
              :initform nil
              :reader file-result-mime-type)))

(defun make-file-result (path &key (mime-type nil))
  (make-instance 'file-result :path path
                              :mime-type mime-type))

(defmethod display ((res file-result))
  (let* ((path (file-result-path res))
         (mime-type (or (file-result-mime-type res) (trivial-mimes:mime path))))
    (if (equal mime-type *plain-text-mime-type*)
      (jsown:new-js
        (mime-type (read-string-file path)))
      (jsown:new-js
        (*plain-text-mime-type* path)
        (mime-type
          (if (or (equal mime-type *svg-mime-type*) (starts-with-p mime-type "text/"))
            (read-string-file path)
            (file-to-base64-string path)))))))

(defclass error-result (result)
  ((ename :initarg :ename
          :reader error-result-ename)
   (evalue :initarg :evalue
           :reader error-result-evalue)
   (quit :initarg :quit
         :initform nil
         :reader error-result-quit)
   (traceback :initarg :traceback
              :initform nil
              :reader error-result-traceback)))

(defun make-error-result (ename evalue &key (quit nil) (traceback nil))
  (make-instance 'error-result :ename ename :evalue evalue
                               :quit quit :traceback traceback))

(defun make-maxima-result (value)
  (if (typep value 'result)
    value
    (cond ((eq (caar value) 'maxima::displayinput)
           (let ((actual-value (third value)))
             (cond ((typep actual-value 'result)
                    actual-value)
                   ((plot-p actual-value)
                    (make-instance 'file-result :path (third actual-value)))
                   (t
                    (make-instance 'mexpr-result :value actual-value)))))
          ((eq (caar value) ':lisp)
            (make-lisp-result (second value))))))

(defun make-lisp-result (value)
  (if (typep value 'result)
    value
    (make-instance 'sexpr-result :value value)))

#|

Convenience functions to return specific types from Lisp or Maxima.

|#

(defun file (path)
  (make-file-result path))

(maxima::defmfun maxima::$mj_file (path)
  (make-file-result path))

(defun text (value)
  (make-inline-result value))

(maxima::defmfun maxima::$mj_text (value)
  (make-inline-result value))

(defun html (value)
  (make-inline-result value :mime-type *html-mime-type*))

(maxima::defmfun maxima::$mj_html (value)
  (make-inline-result value :mime-type *html-mime-type*))

(defun jpeg (value)
  (make-inline-result value :mime-type *jpeg-mime-type*))

(maxima::defmfun maxima::$mj_jpeg (value)
  (make-inline-result value :mime-type *jpeg-mime-type*))

(defun latex (value)
  (make-inline-result value :mime-type *latex-mime-type*))

(maxima::defmfun maxima::$mj_latex (value)
  (make-inline-result value :mime-type *latex-mime-type*))

(defun markdown (value)
  (make-inline-result value :mime-type *markdown-mime-type*))

(maxima::defmfun maxima::$mj_markdown (value)
  (make-inline-result value :mime-type *markdown-mime-type*))

(defun png (value)
  (make-inline-result value :mime-type *png-mime-type*))

(maxima::defmfun maxima::$mj_png (value)
  (make-inline-result value :mime-type *png-mime-type*))

(defun svg (value)
  (make-inline-result value :mime-type *svg-mime-type*))

(maxima::defmfun maxima::$mj_svg (value)
  (make-inline-result value :mime-type *svg-mime-type*))

#|

Jupyter clients generally don't know about the myriad of mime types associated
with TeX/LaTeX and assume that the proper mime type is always text/latex. The
following function will make sure that trivial-mimes database reflects this.

|#

(defun check-mime-db ()
  (iter
    (for ext in '("tex" "latex" "tikz"))
    (setf (gethash ext trivial-mimes:*mime-db*) *latex-mime-type*)))

(check-mime-db)