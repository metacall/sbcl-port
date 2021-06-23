(uiop:define-package :metacall
  (:use #:common-lisp
        #:alexandria
        #:cffi)
  (:import-from :uiop
   :getenv)
  (:export))


(in-package :metacall)

(defun ffi-name-transformer (name kind &key &allow-other-keys)
  (declare (ignorable kind))
  (let* ((lower (string-downcase name))
         #+nil(clean (if (starts-with-subseq "metacall_" lower)
                    (subseq lower 9)
                    lower)))
    (cffi/c2ffi::change-case-to-readtable-case (cffi/c2ffi::camelcase-to-dash-separated lower))))

(uiop/package:define-package #:metacall.ffi (:use))

(common-lisp:in-package #:metacall.ffi)

(cffi:defctype size-t :unsigned-int)

;; Skipped "size_t" due to filters
;; Skipped "wchar_t" due to filters
