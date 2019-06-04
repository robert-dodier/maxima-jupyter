(in-package #:maxima-jupyter)

(defparameter +status-complete+ "complete")
(defparameter +status-incomplete+ "incomplete")
(defparameter +status-invalid+ "invalid")
(defparameter +status-unknown+ "unknown")

(defvar *kernel* nil)
(defvar *page-output* nil)

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
                     :banner "maxima-jupyter: a Maxima Jupyter kernel; (C) 2019 Robert Dodier (BSD)"
                     :language-name "maxima"
                     :language-version maxima::*autoconf-version*
                     :mime-type *maxima-mime-type*
                     :file-extension ".mac"
                     :pygments-lexer "maxima"
                     :codemirror-mode "maxima"
                     :help-links '(("Maxima Reference Manual" . "http://maxima.sourceforge.net/docs/manual/maxima.html")
                                   ("Maxima Documentation" . "http://maxima.sourceforge.net/documentation.html"))))

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
  (when (and (open-stream-p input) (peek-char nil input nil))
    (let ((maxima::*mread-prompt* "")
          (maxima::*prompt-on-read-hang*))
      (declare (special maxima::*mread-prompt*
                        maxima::*prompt-on-read-hang*))
      (maxima::dbm-read input nil))))

(defun my-lread (input)
  (when (and (open-stream-p input) (peek-char nil input nil))
    (read input)))

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
    (handling-errors
      (let ((code-to-eval (if in-maxima
                            (my-mread input)
                            (my-lread input))))
        (if code-to-eval
          (progn
            (jupyter:inform :info kernel "Parsed expression to evaluate: ~W~%" code-to-eval)
            (when in-maxima
              (incf maxima::$linenum)
              (let ((label (maxima::makelabel maxima::$inchar)))
                (unless maxima::$nolabels
                  (setf (symbol-value label) (third code-to-eval)))))
            (let ((result (if in-maxima
                            (my-eval code-to-eval)
                            (eval code-to-eval))))
              (jupyter:inform :info kernel "Evaluated result: ~W~%" result)
              (when (and in-maxima (not (keyword-result-p result)))
                (setq maxima::$% (caddr result)))
              result))
          'no-more-code)))))

(defun make-maxima-label (result)
  (cond
    ((displayinput-result-p result)
      (let ((label (maxima::makelabel maxima::$outchar)))
        (unless maxima::$nolabels
          (setf (symbol-value label) (third result)))
        (make-maxima-result `((maxima::mlabel) ,label ,(third result)))))
    ((lisp-result-p result)
      (jupyter:make-lisp-result (second result)))))

(defmethod jupyter:evaluate-code ((k kernel) code)
  (let ((*kernel* k)
        (maxima::*alt-display1d* #'my-displa)
        (maxima::*alt-display2d* #'my-displa)
        (maxima::*prompt-prefix* (jupyter:kernel-prompt-prefix k))
        (maxima::*prompt-suffix* (jupyter:kernel-prompt-suffix k))
        (maxima::$stdin *query-io*)
        (maxima::$stderr *error-output*)
        (maxima::$stdout *standard-output*))
    (jupyter:inform :info k "eval ~A~%" code)
    (iter
      (with input = (make-string-input-stream code))
      (for in-maxima = (kernel-in-maxima k))
      (for result = (read-and-eval k input in-maxima))
      (until (eq result 'no-more-code))
      (for wrapped-result = (if in-maxima
                              (make-maxima-label result)
                              (jupyter:make-lisp-result result)))
      (when wrapped-result
        (collect wrapped-result))
      (until (jupyter:quit-eval-error-p wrapped-result)))))

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
  (setf (kernel-in-maxima *kernel*) nil)
  (throw 'state-change :no-output))

(defun to-maxima ()
  (setf (kernel-in-maxima *kernel*) t)
  (throw 'state-change :no-output))

(defun my-displa (form)
  (if (mtext-result-p form)
    (let ((maxima::*alt-display1d* nil)
          (maxima::*alt-display2d* nil))
      (maxima::displa form))
    (make-maxima-result form :display-data t :handle t)))

(defun symbol-char-p (c)
  (and (characterp c)
       (or (alphanumericp c)
           (member c '(#\_ #\%)))))

(defun symbol-string-at-position (value pos)
  (let ((start-pos (if (symbol-char-p (char value pos)) pos (if (zerop pos) 0 (1- pos)))))
    (if (symbol-char-p (char value start-pos))
      (let ((start (1+ (or (position-if-not #'symbol-char-p value :end start-pos :from-end t) -1)))
            (end (or (position-if-not #'symbol-char-p value :start start-pos) (length value))))
        (values (subseq value start end) start end))
      (values nil nil nil))))

(defclass inspect-result (jupyter:result)
  ((symbol :initarg :symbol
           :reader inspect-result-symbol)))

(defmethod jupyter:render ((res inspect-result))
  (jsown:new-js
    ("text/plain"
      (string-trim '(#\Newline)
        (with-output-to-string (*standard-output*)
          (cl-info::info-exact (inspect-result-symbol res)))))))

(defmethod jupyter:complete-code ((k kernel) code cursor-pos)
  (if (kernel-in-maxima k)
    (jupyter:handling-errors
      (multiple-value-bind (word start end) (symbol-string-at-position code cursor-pos)
        (when word
          (values
            (let ((name (concatenate 'string "$" (maxima::maybe-invert-string-case word))))
              (with-slots (package) k
                (iter
                  (for sym in-package package)
                  (for sym-name next (symbol-name sym))
                  (when (starts-with-subseq name sym-name)
                    (collect (maxima::print-invert-case (maxima::stripdollar sym-name)))))))
            start
            end))))
    (call-next-method)))

(defmethod jupyter:inspect-code ((k kernel) code cursor-pos detail-level)
  (declare (ignore detail-level))
  (if (kernel-in-maxima k)
    (jupyter:handling-errors
      (multiple-value-bind (word start end) (symbol-string-at-position code cursor-pos)
        (when word
          (jupyter:inform :info k "Inspect ~A~%" word)
          (make-instance 'inspect-result :symbol word))))
    (call-next-method)))
