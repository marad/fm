(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload "fset")
(ql:quickload "marshal")
(ql:quickload "cl-arrows")
(ql:quickload "rail")

(fset:fset-setup-readtable *readtable*)
(use-package 'cl-arrows)
(use-package 'rail)
