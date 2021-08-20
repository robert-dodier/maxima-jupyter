(in-package #:maxima-jupyter)

(defparameter +status-complete+ "complete")
(defparameter +status-incomplete+ "incomplete")
(defparameter +status-invalid+ "invalid")
(defparameter +status-unknown+ "unknown")

(defvar *page-output* nil)
(defvar +abort-report+ "Exit debugger and halt cell execution.")

(defparameter overrides (make-hash-table))

(define-condition maxima-syntax-error (error)
  ((message :initarg :message
            :reader maxima-syntax-error-message))
  (:documentation "Maxima syntax error.")
  (:report (lambda (condition stream)
             (write-string (maxima-syntax-error-message condition) stream))))

(defclass kernel (common-lisp-jupyter:kernel)
  ((in-maxima :initform t
              :accessor kernel-in-maxima))
  (:default-initargs :name "maxima-jupyter"
                     :package (find-package :maxima)
                     :version "0.7"
                     :banner "maxima-jupyter: a Maxima Jupyter kernel; (C) 2019-2021 Robert Dodier (BSD)"
                     :language-name "maxima"
                     :debugger t
                     :language-version maxima::*autoconf-version*
                     :mime-type *maxima-mime-type*
                     :file-extension ".mac"
                     :pygments-lexer "maxima"
                     :codemirror-mode "maxima"
                     :help-links '(("Maxima Reference Manual" . "http://maxima.sourceforge.net/docs/manual/maxima.html")
                                   ("Maxima Documentation" . "http://maxima.sourceforge.net/documentation.html"))))

(defmethod jupyter::start :before ((k kernel))
  (setq maxima::$linenum 0)
  (setq maxima::*display-labels-p* t))

;; This is the entry point for a saved lisp image created by
;; trivial-dump-core:save-executable or equivalent.
(defun kernel-start-exec ()
  ;; IS THERE OTHER STUFF HANDLED BY MAXIMA INIT-CL.LISP THAT WE NEED TO DUPLICATE HERE ??
  (setq *read-default-float-format* 'double-float)
  (jupyter:run-kernel 'kernel (car (uiop:command-line-arguments))))

;;; Based on macro taken from: http://www.cliki.net/REPL
(defmacro handling-errors (&body body)
  `(catch 'maxima::return-from-debugger
    (catch 'maxima::macsyma-quit
      (jupyter:handling-errors ,@body))))

(defun my-mread (input)
  (let ((maxima::*mread-prompt* "")
        (maxima::*prompt-on-read-hang*))
    (declare (special maxima::*mread-prompt*
                      maxima::*prompt-on-read-hang*))
    (or (maxima::mread input) :eof)))


(defun my-lread (input)
  (read input nil :eof))

(defun keyword-lisp-p (code)
  (and (consp code)
       (or (equal ':lisp (car code)) (equal ':lisp-quiet (car code)))))

(defun keyword-command-p (code)
  (and (consp code) (keywordp (car code))))

(defun apply-overrides ()
  (iter
    (for (fun-name handler) in-hashtable overrides)
    (when (fboundp fun-name)
      (eval `(setf (fdefinition (quote ,fun-name))
        (lambda (&rest args)
          (apply ,handler ,(fdefinition fun-name) args))))
      (remhash fun-name overrides))))

(defun my-eval (code)
  (let ((*package* (find-package :maxima)))
    (apply-overrides)
    (cond ((keyword-lisp-p code)
           (cons (list (car code))
                 (multiple-value-list (eval (cons 'progn code)))))
          ((keyword-command-p code)
           (cons (list (car code))
                 (maxima::break-call (car code) (cdr code)
                                     'maxima::break-command)))
          (t
           (setq maxima::$__ (third code))
           (let ((result (maxima::meval* code)))
           (setq maxima::$_ maxima::$__)
           result)))))

(defun read-and-eval (kernel input in-maxima)
  (catch 'state-change
    (let ((code-to-eval (if in-maxima
                          (my-mread input)
                          (my-lread input))))
      (cond
        ((eq :eof code-to-eval)
          :eof)
        (t
          (jupyter:inform :info kernel "Parsed expression to evaluate: ~W~%" code-to-eval)
          (when in-maxima
            (incf maxima::$linenum)
            (setf maxima::*step-next* nil
                  maxima::*break-step* nil)
            (let ((label (maxima::makelabel maxima::$inchar)))
              (unless maxima::$nolabels
                (setf (symbol-value label) (third code-to-eval)))))
          (let ((result (if in-maxima
                          (my-eval code-to-eval)
                          (eval code-to-eval))))
            (jupyter:inform :info kernel "Evaluated result: ~W~%" result)
            (when (and in-maxima (not (keyword-result-p result)))
              (setq maxima::$% (caddr result)))
            result))))))


(defun repl (code source-path breakpoints)
  (with-open-file (input source-path)
    (maxima::get-instream input)
    (let ((maxima::$load_pathname (when source-path (namestring source-path)))
          (maxima::*alt-display1d* #'my-displa)
          (maxima::*alt-display2d* #'my-displa)
          (maxima::*prompt-prefix* (jupyter:kernel-prompt-prefix jupyter:*kernel*))
          (maxima::*prompt-suffix* (jupyter:kernel-prompt-suffix jupyter:*kernel*))
          (maxima::$stdin *query-io*)
          (maxima::$stderr *error-output*)
          (maxima::$stdout *standard-output*))
      (prog (result in-maxima label value)
       repeat
        (setf in-maxima (kernel-in-maxima jupyter:*kernel*)
              result (read-and-eval jupyter:*kernel* input in-maxima))
        (cond
          ((eq result :eof)
            (return))
          ((not in-maxima)
            (jupyter:execute-result result))
          ((lisp-result-p result)
            (jupyter:execute-result (second result)))
          ((not (displayinput-result-p result)))
          ((eq (third result) 'maxima::maxima-error)
            (return (values "maxima-error" (second maxima::$error))))
          (t
            (setf label (maxima::makelabel maxima::$outchar)
                  value `((maxima::mlabel) ,label ,(third result)))
            (unless maxima::$nolabels
              (setf (symbol-value label) (third result)))
            (jupyter:execute-result
              (jupyter:make-mime-bundle
                (list :object-plist
                      *plain-text-mime-type* (mexpr-to-text value)
                      *latex-mime-type* (mexpr-to-latex value)
                      *maxima-mime-type* (mexpr-to-maxima value))))))
        (go repeat)))))


(defmethod jupyter:evaluate-code ((k kernel) code &optional source-path breakpoints)
  (maxima::$debugmode (jupyter:kernel-debugger-started k))
  (if (jupyter:kernel-debugger-started k)
    (restart-bind
        ((abort
           (lambda ()
             (return-from jupyter:evaluate-code (values "ABORT" "Cell execution halted." nil)))
           :report-function (lambda (stream)
                              (write-string +abort-report+ stream))))
      (jupyter:handling-errors (repl code source-path breakpoints)))))


(defmethod jupyter:debug-continue ((k kernel) environment &optional restart-number)
  (cond
    ((not (kernel-in-maxima k))
      (call-next-method))
    (restart-number
      (invoke-restart-interactively (elt (jupyter:debug-environment-restarts environment) restart-number)))
    (t
      (invoke-restart 'continue))))


(defmethod jupyter:debug-next ((k kernel) environment)
  (if (kernel-in-maxima k)
    (invoke-restart 'next)
    (call-next-method)))


(defmethod jupyter:debug-in ((k kernel) environment)
  (if (kernel-in-maxima k)
    (invoke-restart 'into)
    (call-next-method)))


(defun state-change-p (expr)
  (and (listp expr)
       (or (eq (car expr) 'maxima::$to_lisp)
           (eq (car expr) 'maxima::to-maxima)
           (some #'state-change-p expr))))

(defmethod jupyter:code-is-complete ((k kernel) code)
  (handler-case
    (iter
      (with *standard-output* = (make-string-output-stream))
      (with *error-output* = (make-string-output-stream))
      (with input = (make-string-input-stream code))
      (with in-maxima = (kernel-in-maxima k))
      (initially
        (apply-overrides))
      (for char = (peek-char nil input nil))
      (while char)
      (for parsed = (if in-maxima (maxima::dbm-read input nil) (my-lread input)))
      (when (state-change-p parsed)
        (leave +status-unknown+))
      (finally (return +status-complete+)))
    (end-of-file ()
      +status-incomplete+)
    #+sbcl (sb-int:simple-reader-error ()
      +status-incomplete+)
    (simple-condition (err)
      (if (equal (simple-condition-format-control err)
                 "parser: end of file while scanning expression.")
        +status-incomplete+
        +status-invalid+))
    (condition ()
      +status-invalid+)
    (simple-error ()
      +status-invalid+)))

(defun to-lisp ()
  (setf (kernel-in-maxima jupyter:*kernel*) nil)
  (throw 'state-change :no-output))

(defun to-maxima ()
  (setf (kernel-in-maxima jupyter:*kernel*) t)
  (throw 'state-change :no-output))

(defun my-displa (form)
  (if (mtext-result-p form)
    (let ((maxima::*alt-display1d* nil)
          (maxima::*alt-display2d* nil))
      (maxima::displa form))
    (jupyter:display form)))

(defun symbol-char-p (c)
  (and (characterp c)
       (or (alphanumericp c)
           (member c '(#\_ #\%)))))

(defun symbol-string-at-position (value pos)
  (let ((start-pos (if (and (< pos (length value)) (symbol-char-p (char value pos))) pos (if (zerop pos) 0 (1- pos)))))
    (if (symbol-char-p (char value start-pos))
      (let ((start (1+ (or (position-if-not #'symbol-char-p value :end start-pos :from-end t) -1)))
            (end (or (position-if-not #'symbol-char-p value :start start-pos) (length value))))
        (values (subseq value start end) start end))
      (values nil nil nil))))


(defmethod jupyter:complete-code ((k kernel) ms code cursor-pos)
  (if (kernel-in-maxima k)
    (jupyter:handling-errors
      (multiple-value-bind (word start end) (symbol-string-at-position code cursor-pos)
        (when word
          (let ((name (concatenate 'string "$" (maxima::maybe-invert-string-case word))))
            (with-slots (package) k
              (iter
                (for sym in-package package)
                (for sym-name next (symbol-name sym))
                (when (starts-with-subseq name sym-name)
                  (jupyter:match-set-add ms (maxima::print-invert-case (maxima::stripdollar sym-name))
                                            start end
                                            :type (if (fboundp sym) "function" "variable"))))))))
      (values))
    (call-next-method)))


(defmethod jupyter:inspect-code ((k kernel) code cursor-pos detail-level)
  (declare (ignore detail-level))
  (if (kernel-in-maxima k)
    (jupyter:handling-errors
      (multiple-value-bind (word start end) (symbol-string-at-position code cursor-pos)
        (when word
          (jupyter:inform :info k "Inspect ~A~%" word)
          (jupyter:text (string-trim '(#\Newline)
                                     (with-output-to-string (*standard-output*)
                                       (cl-info::info-exact word)))))))
    (call-next-method)))


(defclass debug-frame (jupyter:debug-frame)
  ())


(defclass debug-local-scope (jupyter:debug-scope)
  ()
  (:default-initargs
    :name "Locals"
    :presentation-hint "locals"))


(defclass debug-globals-scope (jupyter:debug-scope)
  ()
  (:default-initargs
    :name "Globals"
    :presentation-hint "globals"))


(defclass debug-variable (jupyter:debug-variable)
  ())


(defun calculate-debug-frames ()
  (do ((i maxima::*current-frame* (1+ i))
       frames)
      (nil)
    (multiple-value-bind (fname vals params backtr lineinfo bdlist)
                         (maxima::frame-info i)
      (declare (ignore vals params backtr bdlist))
      (unless fname
        (return (nreverse frames)))
      (push (make-instance 'debug-frame
                           :data i
                           :source (make-instance 'jupyter:debug-source
                                                  :name (maxima::short-name (cadr lineinfo))
                                                  :path (cadr lineinfo))
                           :line (1+ (car lineinfo))
                           :name (maxima::$sconcat fname))
            frames))))


(defmethod jupyter:debug-object-children-resolve ((instance debug-frame))
  (list (make-instance 'debug-local-scope
                       :environment (jupyter:debug-object-environment instance)
                       :parent instance)))


(defun make-debug-variable (name &optional (value nil value-present-p) (env nil env-present-p))
  (let ((var (make-instance 'debug-variable
                            :name (maxima::$sconcat name))))
    (when value-present-p
      (setf (jupyter:debug-object-value var) (maxima::$sconcat value)
            (jupyter:debug-object-type var) (write-to-string (type-of value))
            (jupyter:debug-object-data var) value))
    (unless (or (typep value 'standard-object)
                (typep value 'structure-object))
      (setf (jupyter:debug-object-id var) 0))
    (when env-present-p
      (setf (jupyter::debug-object-environment var) env)
      (jupyter::register-debug-object var))
    var))


(defmethod jupyter:debug-object-children-resolve ((instance debug-local-scope))
  (multiple-value-bind (fname vals params backtr lineinfo bdlist)
                       (maxima::frame-info (jupyter:debug-object-data (jupyter:debug-object-parent instance)))
    (declare (ignore fname backtr lineinfo bdlist))
    (mapcar #'make-debug-variable params vals)))


(defmethod jupyter:debug-inspect-variables ((kernel kernel) environment)
  (if (kernel-in-maxima kernel)
    (let (variables)
      (setf (fill-pointer (jupyter::debug-environment-objects environment)) 1)
      (dolist (symbol (cdr maxima::$values))
        (push (make-debug-variable symbol (symbol-value symbol) environment) variables))
      (stable-sort variables #'string<
                   :key (lambda (variable)
                          (write-to-string (jupyter:debug-object-name variable)))))
    (call-next-method)))


(defun activate-breakpoint (index)
  (unless (first (elt maxima::*break-points* index))
    (pop (elt maxima::*break-points* index))))


(defun deactivate-breakpoint (index)
  (when (first (elt maxima::*break-points* index))
    (push nil (elt maxima::*break-points* index))))


(defun breakpoint-match-p (source breakpoint index &aux
                           (bp (elt maxima::*break-points* index))
                           (path (namestring source))
                           (line (1- (jupyter:debug-breakpoint-line breakpoint))))
  (values (or (and (null (first bp))
                   (equal path (third bp))
                   (equal line (fourth bp)))
              (and (first bp)
                   (equal path (second bp))
                   (equal line (third bp))))
          (and (first bp) t)))


(defmethod jupyter:debug-remove-breakpoint ((kernel kernel) source breakpoint)
  (j:inform :info kernel "dffg")
  (dotimes (i (length maxima::*break-points*))
    (multiple-value-bind (matches active)
                         (breakpoint-match-p source breakpoint i)
      (j:inform :info kernel "~a ~A~%" matches active)
      (when (and matches active)
        (deactivate-breakpoint i)))))


(defun make-breakpoint (path line)
  (do-symbols (sym "MAXIMA")
    (let ((info (maxima::set-full-lineinfo sym)))
      (when (typep info 'vector)
        (map nil (lambda (form-info)
                   (when (listp form-info)
                     (let ((line-info (maxima::get-lineinfo form-info)))
                       (j:inform :info nil "~a" line-info)
                       (when (and (equal (first line-info) line)
                                  (equal (second line-info) path))
                         (maxima::insert-break-point
                           (maxima::make-bkpt :form form-info
                                              :file-line line
                                              :file path
                                              :function (fourth line-info)))
                         (j:inform :info nil "~a ~a" path line)))))
             info)))))


(defmethod jupyter:debug-activate-breakpoints ((kernel kernel) source breakpoints)
  (declare (ignore kernel))
  (dolist (breakpoint breakpoints)
    (dotimes (i (length maxima::*break-points*)
                (make-breakpoint (namestring source) (1- (jupyter:debug-breakpoint-line breakpoint))))
      (multiple-value-bind (matches active)
                           (breakpoint-match-p source breakpoint i)
        (when matches
          (unless active
            (activate-breakpoint i))
          (return))))))
