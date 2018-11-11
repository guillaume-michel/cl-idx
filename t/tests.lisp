;;;; (asdf:operate 'asdf:test-op :cl-idx)

(in-package :cl-idx-tests)

(define-constant +all-tests+
    '(;; UINT8
      ("uint8_00" (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(0 1 2 3)))
      ("uint8_01" (make-array '(2 4) :element-type '(unsigned-byte 8) :initial-contents '((0 1 2 3)
                                                                                              (4 5 6 7))))
      ("uint8_02" (make-array '(2 2 4) :element-type '(unsigned-byte 8) :initial-contents '(((0 1 2 3) (4 5 6 7))
                                                                                                ((8 9 10 11) (12 13 14 15)))))
      ;; INT8
      ("int8_00" (make-array 4 :element-type '(signed-byte 8) :initial-contents '(0 1 -2 3)))
      ("int8_01" (make-array '(2 4) :element-type '(signed-byte 8) :initial-contents '((0 1 -2 3)
                                                                                           (4 -5 6 7))))
      ("int8_02" (make-array '(2 2 4) :element-type '(signed-byte 8) :initial-contents '(((0 1 -2 3) (4 -5 6 7))
                                                                                             ((8 9 10 11) (12 13 14 15)))))
      ;; INT16
      ("int16_00" (make-array 4 :element-type '(signed-byte 16) :initial-contents '(0 1 -2 3)))
      ("int16_01" (make-array '(2 4) :element-type '(signed-byte 16) :initial-contents '((0 1 -2 3)
                                                                                             (4 -5 6 7))))
      ("int16_02" (make-array '(2 2 4) :element-type '(signed-byte 16) :initial-contents '(((0 1 -2 3) (4 -5 6 7))
                                                                                               ((8 9 10 11) (12 13 14 15)))))
      ;; INT32
      ("int32_00" (make-array 4 :element-type '(signed-byte 32) :initial-contents '(0 1 -2 3)))
      ("int32_01" (make-array '(2 4) :element-type '(signed-byte 32) :initial-contents '((0 1 -2 3)
                                                                                             (4 -5 6 7))))
      ("int32_02" (make-array '(2 2 4) :element-type '(signed-byte 32) :initial-contents '(((0 1 -2 3) (4 -5 6 7))
                                                                                               ((8 9 10 11) (12 13 14 15)))))
      ;; FLOAT32
      ("float32_00" (make-array 4 :element-type 'single-float :initial-contents '(0s0 1s0 -2s0 3s0)))
      ("float32_01" (make-array '(2 4) :element-type 'single-float :initial-contents '((0s0 1s0 -2s0 3s0)
                                                                                           (4s0 -5s0 6s0 7s0))))
      ("float32_02" (make-array '(2 2 4) :element-type 'single-float :initial-contents '(((0s0 1s0 -2s0 3s0) (4s0 -5s0 6s0 7s0))
                                                                                             ((8s0 9s0 10s0 11s0) (12s0 13s0 14s0 15s0)))))
      ;; FLOAT64
      ("float64_00" (make-array 4 :element-type 'double-float :initial-contents '(0d0 1d0 -2d0 3d0)))
      ("float64_01" (make-array '(2 4) :element-type 'double-float :initial-contents '((0d0 1d0 -2d0 3d0)
                                                                                           (4d0 -5d0 6d0 7d0))))
      ("float64_02" (make-array '(2 2 4) :element-type 'double-float :initial-contents '(((0d0 1d0 -2d0 3d0) (4d0 -5d0 6d0 7d0))
                                                                                             ((8d0 9d0 10d0 11d0) (12d0 13d0 14d0 15d0))))))
  :test 'equal
  :documentation "Unit tests definitions.")

(defun path (filename)
  "return full path of file name expressed relativelly to the base project path."
  (asdf:system-relative-pathname :cl-idx-tests filename))

(defun test-file (filename)
  "Given a test idx file name without its extension, returns the full path of the file."
  (path (merge-pathnames "t/data/" (concatenate 'string filename ".idx"))))

(defmacro generate-tests ()
  "Generate FiveAM tests for each test in +all-tests+ (read / write)."
  `(progn ,@(mapcar (lambda (entry)
                      (destructuring-bind (name values) entry
                        `(progn
                           ;; read test
                           (fiveam:test ,(intern (string-upcase (concatenate 'string "test-read-" name)))
                             (fiveam:is (equalp (idx:read-from-file (test-file ,name))
                                                ,values)))
                           ;; write test
                           (fiveam:test ,(intern (string-upcase (concatenate 'string "test-write-" name)))
                             (uiop:with-temporary-file (:stream s :pathname p :direction :io)
                               (idx:write-to-stream s ,values)
                               (fiveam:is (equalp (alexandria:read-file-into-byte-vector (test-file ,name))
                                                  (alexandria:read-file-into-byte-vector p))))))))
                    +all-tests+)))

(fiveam:def-suite cl-idx-test-suite
    :description "cl-idx test suite.")

(fiveam:in-suite cl-idx-test-suite)

(generate-tests)

(defun run-tests ()
  (princ "Running all cl-idx unit tests")
  (fiveam:run! 'cl-idx-test-suite))
