(require :folder-manager)

(defun run ()
  (folder-manager:main *posix-argv*))

(sb-ext:save-lisp-and-die "fm" :toplevel #'run :executable t)
