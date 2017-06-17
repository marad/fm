;; Managed directory
(defvar *dir* "/home/morti/dev/")

;; Maximum level of nesting projects
(defvar *max-nesting-level* 3)

;; TOOLS

(defun flatten (ls)
  (labels ((mklist (x) (if (listp x) x (list x))))
    (mapcan #'(lambda (x) (if (atom x) (mklist x) (flatten x))) ls)))

(defun list->set (lst)
  (reduce #'fset:with lst :initial-value #{}))

(defun set->list (set)
  (fset:reduce #'(lambda (l e) (cons e l)) set :initial-value nil))

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

(defun current-project-name ()
  (project-name (current-project-folder)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Metadata

(defun fman-meta-path (project-path)
  "Given project path, returns project metadata file path"
  (make-pathname :defaults project-path :name "fman" :type "cl"))

(defun ensure-set (value)
  (if (equal (type-of value) 'fset:wb-set)
      value #{value}))

(defclass metadata ()
    ((name :accessor meta-name
           :initform nil
           :initarg :name)
     (path :accessor meta-path
           :initform nil
           :initarg :path)
     (tags :accessor meta-tags
           :initform (fset:empty-set)
           :initarg :tags)))

(defmethod print-object ((o metadata) stream)
  (format stream "Metadata {~% name: ~a~% path: ~a~% tags: ~a~%}"
          (meta-name o) (meta-path o) (meta-tags o)))

(defun make-metadata (name path tags)
  (make-instance 'metadata :name name :path path :tags tags))

(defmethod meta-has-tags ((meta metadata) tags)
  (or (null tags)
      (fset:subset? (ensure-set tags) (meta-tags meta))))

(defmethod meta-add-tags ((meta metadata) tags)
  (setf (meta-tags meta)
        (fset:union (meta-tags meta)
                    (ensure-set tags)))
  meta)


(defmethod meta-remove-tags ((meta metadata) tags)
  (setf (meta-tags meta)
        (fset:set-difference (meta-tags meta)
                             (ensure-set tags)))
  meta)

(defmethod ms:class-persistant-slots ((self metadata))
  '(tags))

(defmethod write-metadata ((meta metadata) project-path)
  "Saves project metadata to file in project"
  (with-open-file (out (fman-meta-path project-path)
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print (ms:marshal (meta-tags meta)) out))))

(defun get-file-contents (filename)
  (with-open-file (in filename
                      :if-does-not-exist nil)
    (->> (fail-if-nil :file-not-found in)
         (fmap (lambda (stream)
                 (let ((contents (-> stream file-length make-string)))
                   (read-sequence contents stream)
                   contents))))))

(defun read-metadata (project-path)
  "Read project metadata"
  (make-metadata (project-name project-path)
                 project-path
                 (->> (fman-meta-path project-path)
                      (get-file-contents)
                      (fmap #'read-from-string)
                      (fmap #'ms:unmarshal)
                      (fmap #'eval)
                      (get-or-default #{}))))

(defun all-metas ()
  (mapcar #'read-metadata (list-project-folders)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handler Functions


(defun find-project (&key (tags nil))
  "Finds projects with given tags. Tags may be either single tag or set of tags"
  (remove-if-not #'(lambda (meta) (meta-has-tags meta tags))
                 (all-metas)))

(defun find-by-name (name)
  "Finds project by name"
  (first (remove-if-not #'(lambda (meta) (equal (meta-name meta) name))
                        (all-metas))))

(defun project-path (name)
  "Returns project path by name"
  (meta-path (find-by-name name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Printing data

(defun print-name (metadata)
  (format t "~a~%" (meta-name metadata)))

(defun print-meta (metadata)
  (format t "Name: ~a~%Path: ~a~%Tags: ~{~a ~}~%"
          (meta-name metadata)
          (meta-path metadata)
          (set->list (meta-tags metadata))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLI Interface

(defun cli-command ()
  (cadr *posix-argv*))

(defun handle-ls (args)
  (format t "~{~a~%~}" (mapcar #'meta-name (find-project :tags (list->set args)))))

(defun handle-path (args)
  (let ((path (if (null args)
                  (current-project-folder)
                  (meta-path (find-by-name (car args))))))
    (format t "~a~%" path)))

(defun handle-meta ()
  (print-meta (read-metadata (current-project-folder))))

(defun handle-tag-add (tags)
  (let ((meta (read-metadata (current-project-folder))))
    (write-metadata (meta-add-tags meta (list->set tags))
                    (current-project-folder))))

(defun handle-tag-rm (tags)
  (let ((meta (read-metadata (current-project-folder))))
    (write-metadata (meta-remove-tags meta (list->set tags))
                    (current-project-folder))))

(defun handle-show-tags ()
  (let ((meta (read-metadata (current-project-folder))))
    (format t "~{~a ~}" (set->list (meta-tags meta)))
    (format t "~%")))

(defun handle-tag (args)
  (let ((cmd (car args))
        (tags (cdr args)))
    (cond ((equal cmd "add") (handle-tag-add tags))
          ((equal cmd "rm") (handle-tag-rm tags))
          ((null tags) (handle-show-tags))
          (t (format t "Unknown command!~%")))))

(defun print-help ()
  (format t "Available commands:
ls - lists all managed projects
path - path for given project name
meta - show metadata information for project
tag - manage project tags
help - show this message~%"))

(defun handle-command (command-args)
  (let ((cmd (car command-args))
        (args (cdr command-args)))
    (cond ((equal cmd "ls") (handle-ls args))
          ((equal cmd "path") (handle-path args))
          ((equal cmd "meta") (handle-meta))
          ((equal cmd "tag") (handle-tag args))
          ((equal cmd "help") (print-help))
          (t (print-help)))))

(defun main ()
  (handle-command (cdr *posix-argv*)))
