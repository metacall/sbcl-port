(in-package :metacall)

;;;;;;
;;; utils
#+sbcl
(defun setenv (name value)
  #+sbcl
  (sb-posix:setenv name value 1)
  #-sbcl
  (error "Implement setenv for this platform"))

(defun append-to-path (env-var &rest args)
  (setenv env-var (hu.dwim.util:string+ (getenv env-var) ":" (apply #'hu.dwim.util:string+ args))))

(defcfun "strlen" :int
  "Calculate the length of a string."
  (n :string))

(defvar *base-dir* (uiop:native-namestring (asdf:system-relative-pathname :metacall "")))

;;;;;;
;;; API

(defvar *log* nil)

;; TODO: initialise with provided absolute and relative paths
(defun init ()
  (unless *log*
    (progn
      (append-to-path "LOADER_SCRIPT_PATH" *base-dir* "script/js/node_modules")
      (append-to-path "LOADER_SCRIPT_PATH" *base-dir* "script/js")
      (append-to-path "LOADER_SCRIPT_PATH" *base-dir* "script/py")
      (metacall.ffi::metacall-initialize)
      (setf *log* (foreign-alloc '(:struct metacall-log-stdio-type)))
      (metacall.ffi::metacall-log metacall-log-stdio *log*))))

(defun cleanup ()
  (foreign-free *log*)
  (setf *log* nil))

(defun load-script (type script)
  (cffi:with-foreign-object (fscripts :string 1)
    (cffi:with-foreign-strings ((tag type)
                                (fscript script))
      (setf (cffi:mem-aref fscripts :string 0) fscript)
      (sb-int:set-floating-point-modes :traps nil)
      (metacall-load-from-file tag fscripts 1 (cffi:null-pointer)))))

(defun %value-create (arg)
  (etypecase arg
    ;; value-create-int
    ;; value-create-map
    ;; value-create-ptr
    ;; value-create-bool
    ;; value-create-char
    ;; value-create-long
    ;; value-create-null
    ;; value-create-array
    ;; value-create-class
    ;; value-create-float
    ;; value-create-short
    ;; value-create-buffer
    ;; value-create-double
    ;; value-create-future
    ;; value-create-object
    ;; value-create-string
    ;; value-create-function
    ;; value-create-function-closure
    (simple-base-string (with-foreign-string (str arg) (metacall-value-create-string str (strlen str))))
    (bit (metacall-value-create-int (coerce arg 'integer)))
    (integer (metacall-value-create-long arg))
    (single-float (metacall-value-create-double (coerce arg 'double-float)))
    (double-float (metacall-value-create-double arg))
    (null (metacall-value-create-null))
    (ratio (metacall-value-create-double (coerce arg 'double-float)))))

(defun %call (function &rest args)
  (with-foreign-object (fargs :pointer (length args))
    (loop for arg in args
          ;; TODO: validation of type and type/arg correspondence
          for i from 0
          do
             (setf (mem-aref fargs '(:pointer :void) i) (%value-create arg)))
    (let* ((v (metacall.ffi::metacallv function (if args fargs (null-pointer))))
           (type (metacall-value-id v)))
      (if (eq 'metacall-invalid type)
          (error "An error occurred while calling ~A with args ~A" function args)
          (values v type)))))

(defmacro with-allocator ((name) &body forms)
  "Creates a metacall allocator called name initially, and frees it after the body"
  (with-unique-names (ctx)
    `(with-foreign-objects ((,ctx '(:struct metacall-allocator-std-type))
                            (,name '(:pointer :void)))
       (unwind-protect
            (progn
              (setf ,name (null-pointer))
              (with-foreign-slots ((malloc realloc free) ,ctx (:struct metacall-allocator-std-type))
                (setf malloc (cffi:foreign-symbol-pointer "malloc")
                      realloc (cffi:foreign-symbol-pointer "realloc")
                      free (cffi:foreign-symbol-pointer "free")))
              (setf ,name (metacall-allocator-create metacall-allocator-std ,ctx))
              ,@forms)
         (unless (null-pointer-p ,name)
           (metacall-allocator-destroy ,name))))))
