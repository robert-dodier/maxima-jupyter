(in-package #:maxima-jupyter)

;; nicked from: https://rosettacode.org/wiki/String_matching#Common_Lisp
(defun starts-with-p (str1 str2)
  (let ((p (search str2 str1)))
    (and p (= 0 p))))

;; nicked from: https://rosettacode.org/wiki/String_matching#Common_Lisp
(defun ends-with-p (str1 str2)
  (let ((p (mismatch str2 str1 :from-end T)))
    (or (not p) (= 0 p))))
