(in-package #:maxima-jupyter)

#|

Standard MIME types

|#

(defvar *latex-mime-type* "text/latex")
(defvar *maxima-mime-type* "text/x-maxima")
(defvar *plain-text-mime-type* "text/plain")


(defun keyword-result-p (code)
  (and (listp code)
       (listp (car code))
       (keywordp (caar code))))

(defun lisp-result-p (code)
  (and (listp code)
       (listp (car code))
       (eq (caar code) ':lisp)))

(defun mlabel-result-p (code)
  (and (listp code)
       (listp (car code))
       (eq (caar code) 'maxima::displayinput)))

(defun displayinput-result-p (code)
  (and (listp code)
       (listp (car code))
       (eq (caar code) 'maxima::displayinput)))

(defun mlabel-input-result-p (code)
  (and (listp code)
       (listp (car code))
       (eq (caar code) 'maxima::mlabel)
       (starts-with-subseq (string maxima::$inchar) (string (second code)))))

(defun mtext-result-p (code)
  (and (listp code)
         (listp (car code))
         (eq (caar code) 'maxima::mtext)))

(defun plot-p (value)
  (and (listp value)
       (eq (caar value) 'maxima::mlist)
       (eq (list-length value) 3)
       (stringp (second value))
       (stringp (third value))
       (or (ends-with-subseq ".gnuplot" (second value))
           (ends-with-subseq ".gnuplot_pipes" (second value)))))

(defun sexpr-to-text (value)
  (format nil "~S" value))

(defun mexpr-to-text (value)
  (string-trim '(#\Newline)
               (with-output-to-string (*standard-output*)
                 (let ((maxima::*alt-display1d* nil)
                       (maxima::*alt-display2d* nil))
                   (maxima::displa value)))))

(defun mexpr-to-latex (value)
  (let ((env (maxima::get-tex-environment value)))
    (apply #'concatenate 'string
           (mapcar #'string
                   (maxima::tex value
                                (list (car env)) (list (cdr env))
                                'maxima::mparen 'maxima::mparen)))))

(defun mexpr-to-maxima (value)
  (let ((maxima::*display-labels-p* nil))
    (with-output-to-string (f)
      (maxima::mgrind value f))))

(defclass mexpr-result (jupyter:result)
  ((value :initarg :value
          :reader mexpr-result-value)))

(defmethod jupyter:render ((res mexpr-result))
  (let ((value (mexpr-result-value res)))
    (if (mlabel-input-result-p value)
      `(:object-plist
         ,*plain-text-mime-type* ,(mexpr-to-text value)
         ,*maxima-mime-type* ,(mexpr-to-maxima value))
      `(:object-plist
         ,*plain-text-mime-type* ,(mexpr-to-text value)
         ,*latex-mime-type* ,(mexpr-to-latex value)
         ,*maxima-mime-type* ,(mexpr-to-maxima value)))))

(defun make-maxima-result (value &key (display-data nil) (handle nil))
  (let ((result (cond ((typep value 'jupyter:result)
                        value)
                      ((eq value 'maxima::maxima-error)
                        (jupyter:make-error-result "maxima-error" (second maxima::$error)))
                      ((lisp-result-p value)
                        (jupyter:make-lisp-result (second value)))
                      ((and (mlabel-result-p value) (typep (third value) 'jupyter:result))
                        (third value))
                      (t
                        (make-instance 'mexpr-result :value value :display-data display-data)))))
    (if (and handle display-data)
      (progn
        (jupyter:send-result result)
        t)
      result)))
