(asdf:defsystem #:maxima-jupyter
  :description "An Enhanced Interactive Shell for Maxima."
  :version "0.6"
  :author "Robert Dodier"
  :license "BSD 2-Clause. See LICENSE."
  :depends-on (:alexandria
               :common-lisp-jupyter
               :iterate)
  :components
    ((:static-file "load-maxima-jupyter.lisp")
     (:module res
      :components
        ((:static-file "logo-64x64.png")))
     (:module src
      :serial t
      :components ((:file "packages")
                   (:file "results")
                   (:file "kernel")
                   (:file "installer")
                   (:file "additions")
                   (:file "overrides")
                   (:file "overrides-cl-info")))))
