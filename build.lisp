#!/usr/bin/sbcl --script

;;(defun create-exe ())
(load "setup.lisp")
(load "fm.lisp")
(sb-ext:save-lisp-and-die "fm" :toplevel #'main :executable t)
