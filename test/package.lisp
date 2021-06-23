(in-package :metacall)

(defpackage :metacall/test
  (:use :common-lisp
        :hu.dwim.stefil
        :metacall
        :metacall.ffi))

(import-all-owned-symbols :metacall.ffi :metacall/test)
(import-all-owned-symbols :metacall :metacall/test)
