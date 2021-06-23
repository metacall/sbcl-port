(defsystem :metacall
  :description "Common Lisp FFI wrapper for metacall, https://metacall.io/"
  :author "Kambiz Darabi"
  :license "Apache 2.0"
  :version "0.1"

  :defsystem-depends-on (:cffi/c2ffi)
  :depends-on (:alexandria
               :cffi
               :cffi/c2ffi
               :cffi-libffi)
  :components ((:file "package-stage-1"
                :pathname "source/package-stage-1")
               (:module "source"
                :depends-on ("c2ffi-spec" "package-stage-1")
                :serial t
                :components ((:file "package-stage-2")
                             (:file "package-stage-3")
                             (:file "metacall")))
               (:module "c2ffi-spec"
                :depends-on ("package-stage-1")
                :components ((:cffi/c2ffi-file "metacall.h"
                              :package #:metacall.ffi
                              :ffi-name-transformer "metacall::ffi-name-transformer"
                              :foreign-library-name "metacall::metacall"
                              :foreign-library-spec ((t (:default "libmetacall")))
                              :include-sources ("metacall_allocator\\.h$"
                                                "metacall_api\\.h$"
                                                "metacall_def\\.h$"
                                                "metacall_features\\.h$"
                                                "metacall_fork\\.h$"
                                                "metacall\\.h$"
                                                "metacall_loaders\\.h$"
                                                "metacall_log\\.h$"
                                                "metacall_value\\.h$"
                                                "metacall_version\\.h$"
                                                )
                              :exclude-sources :all
                              :include-definitions :all
                              :exclude-definitions nil)))))

(defsystem :metacall/test
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.test-system"
  :depends-on (:hu.dwim.stefil
               :metacall)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "suite" :depends-on ("package"))
                             (:file "basic" :depends-on ("suite"))))))
