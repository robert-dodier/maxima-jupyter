(in-package #:maxima)

#|

Utility function to assist with overrides including dynamic overrides.

|#

(defmacro defover (fun-name lambda-list &body body)
  `(setf (gethash (quote ,fun-name) maxima-jupyter::overrides)
     (lambda ,lambda-list ,@body)))


#|

src/init-cl.lisp Overrides

to_lisp and to-maxima are overridden since we do not have true Maxima REPL. We
set a flag and throw out of the current evaluation.

|#

(defover $to_lisp (orig &rest args)
  (format t "~&Type (to-maxima) to restart, ($quit) to quit Maxima.~%")
  (maxima-jupyter::to-lisp))

(defover to-maxima (orig &rest args)
  (format t "Returning to Maxima~%")
  (maxima-jupyter::to-maxima))


#|

src/mactex.lisp Overrides

The environments for mdefine. mdefmacro and mlabel are set to the default math
environment since verbatim doesn't understand math mode.

|#

(setq *tex-environment-default* '("\\[" . "\\]"))

(setf (get 'mdefine 'tex-environment) *tex-environment-default*)

(setf (get 'mdefmacro 'tex-environment) *tex-environment-default*)

(setf (get 'mlabel 'tex-environment) *tex-environment-default*)

#|

tex-mlabel is overridden to use tag so that mlabel will show up correctly.

|#

(defprop mlabel jupyter-mlabel tex)

(defun jupyter-mlabel (x l r)
  (tex (caddr x)
       (append l
	       (if (and (cadr x) *display-labels-p*)
		   (list (format nil "\\tag{$~A$}" (tex-stripdollar (cadr x))))
		   nil))
       r 'mparen 'mparen))


#|

src/mload.lisp Overrides

batch is overridden to catch the demo flag and use the set_next_input payload
to send the code to the client.

|#

(defover $batch (orig filename &optional (demo :batch))
  (if (eql demo '$demo)
    (with-open-file (in-stream filename)
      (format t (intl:gettext "~%read and interpret file: ~A~%")
        (truename in-stream))
      (loop
        for expr = (dbm-read in-stream nil) then (dbm-read in-stream nil)
        while expr
        do
        (jupyter:enqueue-input maxima-jupyter::*kernel*
          (with-output-to-string (f)
            (mgrind (third expr) f)
            (write-char (if (eql (caar expr) 'displayinput) #\; #\$) f)))))
    (funcall orig filename demo)))


#|

src/nparse.lisp Overrides

mread-synerr is overridden so that a condition is created instead of a throw.
The original is not included since we call it directly.

|#

(defover mread-synerr (orig &rest args)
  (error (make-condition 'maxima-jupyter::maxima-syntax-error :message
    (with-output-to-string (*standard-output*)
      (catch 'macsyma-quit
        (apply orig args))))))


#|

src/plot.lisp Overrides

$plot2d is overridden so that any results are displayed immediately, even if the
plot is inside of a block.

|#

(defover $plot2d (orig &rest args)
  (let ((value (apply orig args)))
    (when (maxima-jupyter::plot-p value)
      (jupyter-file (third value) t))
    value))

#|

$plot3d is overridden so that any results are displayed immediately, even if the
plot is inside of a block.

|#

(defover $plot3d (orig &rest args)
  (let ((value (apply orig args)))
    (when (maxima-jupyter::plot-p value)
      (jupyter-file (third value) t))
    value))


#|

src/suprv1.lisp Overrides

$batchload is overridden in case of an autoload that defines new functions.

|#

(defover $batchload (orig &rest args)
  (let ((result (apply orig args)))
    (maxima-jupyter::apply-overrides)
    result))

#|

loadfile is overridden in case of an autoload that defines new functions.

|#

(defover loadfile (orig &rest args)
  (let ((result (apply orig args)))
    (maxima-jupyter::apply-overrides)
    result))

#|

$quit is overridden send an ask_exit payload to the frontend. This will only
work in the console and the Qt console. In JupyterLab this we be ignored,
including by "terminal" sessions.

|#

(defover $quit (orig &rest args)
  (jupyter:quit))


#|

share/contrib/implicit_plot.lisp Overrides

$implicit_plot is overridden so that any files that are created can be
displayed.

|#

(defover $implicit_plot (orig expr xrange yrange &rest extra-options)
  (let ((result (apply orig expr xrange yrange extra-options))
        (options (copy-tree *plot-options*)))
    (setf (getf options :type) "plot2d")
    (setq options (plot-options-parser extra-options options))
    (unless (eq (getf options :plot_format) '$xmaxima)
      (let ((pdf-file (getf options :pdf_file))
            (png-file (getf options :png_file))
            (ps-file (getf options :ps_file))
            (svg-file (getf options :svg_file))
            (out-file (getf options :gnuplot_out_file))
            (term (getf options :gnuplot_term)))
        (cond (pdf-file
                (jupyter-pdf-file (plot-file-path pdf-file) t))
              (png-file
                (jupyter-png-file (plot-file-path png-file) t))
              (ps-file
                (jupyter-ps-file (plot-file-path ps-file) t))
              (svg-file
                (jupyter-svg-file (plot-file-path svg-file) t))
              (t
                (when out-file
                  (case term
                    ($gif
                      (jupyter-gif-file (plot-file-path out-file) t))
                    ($jpeg
                      (jupyter-jpeg-file (plot-file-path out-file) t))
                    ($ps
                      (jupyter-ps-file (plot-file-path out-file) t))
                    (($pdf $pdfcairo)
                      (jupyter-pdf-file (plot-file-path out-file) t))))))))
    result))


#|

share/draw/gnuplot.lisp Overrides

draw_gnuplot is overridden so that any files that are created can be displayed.

|#

(defun get-draw-file-name (extension)
  (namestring
    (merge-pathnames
      (concatenate 'string (funcall 'get-option '$file_name)
                           extension)
      (uiop:getcwd))))

(defover draw_gnuplot (orig &rest args)
  (let ((result (apply orig args)))
    (case (funcall 'get-option '$terminal)
      (($pdf $multipage_pdf $pdfcairo $multipage_pdfcairo)
        (jupyter-file (get-draw-file-name ".pdf") t))
      (($gif $animated_gif)
        (jupyter-file (get-draw-file-name ".gif") t))
      (($png $pngcairo)
        (jupyter-file (get-draw-file-name ".png") t))
      (($eps $multipage_eps $eps_color $multipage_eps_color $epslatex $epslatex_standalone)
        (jupyter-file (get-draw-file-name ".eps") t))
      ($jpg
        (jupyter-file (get-draw-file-name ".jpg") t))
      ($svg
        (jupyter-file (get-draw-file-name ".svg") t)))
    result))


#|

share/dynamics/complex_dynamics.lisp Overrides

$mandelbrot is overridden so that any results are displayed immediately, even if
the plot is inside of a block.

|#

(defover $mandelbrot (orig &rest args)
  (let ((value (apply orig args)))
    (when (maxima-jupyter::plot-p value)
      (jupyter-file (third value) t))
    value))

#|

$julia is overridden so that any results are displayed immediately, even if the
plot is inside of a block.

|#

(defover $julia (orig &rest args)
  (let ((value (apply orig args)))
    (when (maxima-jupyter::plot-p value)
      (jupyter-file (third value) t))
    value))

#|

$print is overridden so that math is displayed inline.

|#

(defover $print (orig &rest args)
  (jupyter:latex
    (format nil "~{~a~^ ~}"
                (mapcar (lambda (arg) (if (stringp arg) arg (format nil "\\(~A\\)" ($tex1 arg))))
                        args))
    :display t)
  (car (last args)))


(defun break-dbm-loop (at)
  (let* ((*quit-tags* (cons (cons *break-level* *quit-tag*) *quit-tags*))
         (*break-level* (if (not at) *break-level* (cons t *break-level*)))
         (*quit-tag* (cons nil nil))
         (*break-env* *break-env*)
         (*mread-prompt* "")
         (*diff-bindlist* nil)
         (*diff-mspeclist* nil))
    (unwind-protect
        (restart-bind
          ((cl:continue
             (lambda ()
               (return-from break-dbm-loop :resume))
             :report-function (lambda (stream)
                                (write-string "Continue evaluation" stream)))
           (maxima-jupyter::next
             (lambda ()
               (step-next)
               (return-from break-dbm-loop :resume))
             :report-function (lambda (stream)
                                (write-string "Step next" stream)))
           (maxima-jupyter::into
             (lambda ()
               (step-into)
               (return-from break-dbm-loop :resume))
             :report-function (lambda (stream)
                                (write-string "Step into" stream))))
          (let ((environment (make-instance 'jupyter:debug-environment
                                            :condition nil
                                            :restarts (compute-restarts)
                                            :frames (maxima-jupyter::calculate-debug-frames))))
            (unless at
              (break-frame 0 nil))
            (jupyter:debug-stop "exception" environment)))
      (restore-bindings)))
  :resume)

