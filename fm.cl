#!/usr/bin/sbcl --script
;; Managed directory
(defvar *dir* "/home/morti/dev/")

;; Maximum level of nesting projects
(defvar *max-nesting-level* 2)

;; TOOLS

(defun flatten (ls)
  (labels ((mklist (x) (if (listp x) x (list x))))
    (mapcan #'(lambda (x) (if (atom x) (mklist x) (flatten x))) ls)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project folders

(defun project-name (project-path)
  (car (last (pathname-directory project-path))))

(defun is-project (project-path)
  (probe-file (make-pathname :defaults project-path :name ".git")))

(defun scan-directory (dir &optional (level 1))
  "Scans directory for projects recursively"
  (if (> level *max-nesting-level*)
    nil
    (if (is-project dir)
      dir
      (loop for f in (directory (make-pathname :defaults dir :name :wild))
            collect (scan-directory f (+ level 1))))))

(defun list-project-folders ()
  "Lists all projects at *dir* up to *max-nesting-level* folders deep"
  (flatten (scan-directory *dir*)))

(defun current-project-folder ()
  (first (directory "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Metadata

(defun fman-meta-path (project-path) 
  "Given project path, returns project metadata file path"
  (make-pathname :defaults project-path :name "fman" :type "cl"))

(defun metadata-clear (metadata)
  "Removes all but supported metadata information"
  (remove-if-not #'(lambda (pair)
                     (member (car pair) '(:tags))) metadata))

(defun metadata-name (metadata)
  (cdr (assoc :name metadata)))

(defun metadata-path (metadata)
  (cdr (assoc :path metadata)))

(defun metadata-tags (metadata)
  (cdr (assoc :tags metadata)))

(defun metadata-set (metadata key value)
  (acons key value (remove-if
                          #'(lambda (pair)
                              (eql (car pair) key))
                          metadata)))

(defun metadata-set-name (metadata value)
  (metadata-set metadata :name value))

(defun metadata-set-path (metadata value)
  (metadata-set metadata :path value))

(defun metadata-has-tag (metadata tag)
  (member tag (metadata-tags metadata) :test #'equal))

(defun metadata-has-tags (metadata tags)
  (not (set-difference tags (metadata-tags metadata) :test #'equal)))

(defun metadata-add-tag (metadata tag)
  (metadata-set metadata :tags
                (cons tag (metadata-tags metadata))))

(defun metadata-add-tags (metadata tags)
  (metadata-set metadata :tags
                (append (metadata-tags metadata) tags)))

(defun metadata-remove-tag (metadata tag)
  (metadata-set metadata :tags
                (remove tag (metadata-tags metadata) :test #'equal)))

(defun metadata-remove-tags (metadata tags)
  (metadata-set metadata :tags
                (remove-if #'(lambda (tag) (member tag tags :test #'equal))
                           (metadata-tags metadata))))


(defun write-project-metadata (project-path meta)
  "Saves project metadata to file in project"
  (with-open-file (out (fman-meta-path project-path) 
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print (metadata-clear meta) out))))

(defun read-project-metadata (project-path)
  "Reads project metadata :)"
  (with-open-file (in (fman-meta-path project-path)
                      :if-does-not-exist nil)
    (acons :name (project-name project-path)
           (acons :path project-path
                  (if (null in)
                    nil
                    (with-standard-io-syntax
                      (read in)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handler Functions

(defun find-project (&key (tags nil))
  "Finds projects with given tags"
  (remove-if-not #'(lambda (meta) (metadata-has-tags meta tags))
                 (mapcar #'read-project-metadata (list-project-folders))))

(defun find-by-name (name)
  "Finds project by name"
  (first (remove-if-not #'(lambda (meta) (equal (metadata-name meta) name))
                        (mapcar #'read-project-metadata (list-project-folders)))))

(defun project-path (name)
  "Returns project path by name"
  (metadata-path (find-by-name name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Printing data

(defun print-name (metadata)
  (format t "~a~%" (metadata-name metadata)))

(defun print-meta (metadata)
  (loop for (a . b) in metadata
        do (format t "~a: ~a~%" a b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLI Interface

(defun cli-command ()
  (cadr *posix-argv*))

(defun handle-ls (args)
  (format t "~{~a~%~}~%" (mapcar #'metadata-name (find-project :tags args))))

(defun handle-cd (args)
  (format t "Handling cd with ~a~%" args))

(defun handle-meta ()
  (print-meta (read-project-metadata (current-project-folder))))


(defun handle-tag-add (tags)
  (let ((meta (read-project-metadata (current-project-folder))))
    (write-project-metadata (current-project-folder)
                            (metadata-add-tags meta tags))))

(defun handle-tag-rm (tags)
  (let ((meta (read-project-metadata (current-project-folder))))
    (write-project-metadata (current-project-folder)
                            (metadata-remove-tags meta tags))))

(defun handle-tag (args)
  (let ((cmd (car args))
        (tags (cdr args)))
    (cond ((equal cmd "add") (handle-tag-add tags))
          ((equal cmd "rm") (handle-tag-rm tags))
          (t (format t "Unknown command!~%")))))

(defun handle-command (command-args)
  (let ((cmd (car command-args))
        (args (cdr command-args)))
    (cond ((equal cmd "ls") (handle-ls args))
          ((equal cmd "cd") (handle-cd args))
          ((equal cmd "meta") (handle-meta))
          ((equal cmd "tag") (handle-tag args))
          (t (format t "Unknown command!~%")))))

(defun main ()
  (handle-command (cdr *posix-argv*)))

(main)
