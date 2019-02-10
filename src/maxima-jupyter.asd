(asdf:defsystem #:maxima-jupyter
  :description "An Enhanced Interactive Shell for Common Lisp (based on the Jupyter protocol)."
  :version "0.6"
  :author "Frederic Peschanski (format nil \"<frederic~Apeschanski~Awork~Agmail~Acom>\" \".\" \".\" \"@\" \".\")"
  :license "BSD 2-Clause. See LICENSE."
  :depends-on (:common-lisp-jupyter
               :iterate)
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "results")
               (:file "kernel")
               (:file "additions")
               (:file "overrides")
               (:file "overrides-cl-info")))
