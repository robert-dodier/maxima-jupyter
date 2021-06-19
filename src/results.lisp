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

