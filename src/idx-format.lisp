(in-package #:cl-idx)

;; the IDX file format is a simple format for vectors and multidimensional matrices of various numerical types.

;; The basic format is

;; magic number
;; size in dimension 0
;; size in dimension 1
;; size in dimension 2
;; .....
;; size in dimension N
;; data

;; The magic number is an integer (MSB first). The first 2 bytes are always 0.

;; The third byte codes the type of the data:
;; 0x08: unsigned byte
;; 0x09: signed byte
;; 0x0B: short (2 bytes)
;; 0x0C: int (4 bytes)
;; 0x0D: float (4 bytes)
;; 0x0E: double (8 bytes)

;; The 4-th byte codes the number of dimensions of the vector/matrix: 1 for vectors, 2 for matrices....

;; The sizes in each dimension are 4-byte integers (MSB first, high endian, like in most non-Intel processors).

;; The data is stored like in a C array, i.e. the index in the last dimension changes the fastest.

(define-constant +idx-types+
    '((#x08 (unsigned-byte 8) uint8)
      (#x09 (signed-byte 8) int8)
      (#x0B (signed-byte 16) int16)
      (#x0C (signed-byte 32) int32)
      (#x0D single-float float)
      (#x0E double-float double))
  :test 'equal)

(define-constant +idx-revert-types+
    '(((unsigned-byte 8) #x08)
      ((signed-byte 8) #x09)
      ((signed-byte 16) #x0B)
      ((signed-byte 32) #x0C)
      (single-float #x0D)
      (double-float #x0E))
  :test 'equalp)

(defun get-dtype (data)
  (nth 1 (assoc (array-element-type data) +idx-revert-types+ :test #'equalp)))

(defun symbol-append (&rest symbols)
  (intern (apply #'concatenate 'string
                 (mapcar #'symbol-name symbols))
          :cl-idx))

(defun element-type (dtype)
  (nth 1 (assoc dtype +idx-types+)))

(defun get-fn (dtype prefix)
  (symbol-function (symbol-append prefix (nth 2 (assoc dtype +idx-types+)))))

(defun get-read-fn (dtype)
  (get-fn dtype 'read-))

(defun get-write-fn (dtype)
  (get-fn dtype 'write-))

(defun signed->unsigned (size value)
  (ldb (byte size 0) value))

(defun unsigned->signed (size value)
  (if (logbitp (1- size) value)
      (dpb value (byte size 0) -1)
      value))

(defun read-uint8 (str)
  (read-byte str))

(defun write-uint8 (value str)
  (write-byte value str))

(defun read-int8 (str)
  (unsigned->signed 8 (read-byte str)))

(defun write-int8 (value str)
  (write-byte (signed->unsigned 8 value) str))

(defun read-uint16 (str)
  (nibbles:read-ub16/be str))

(defun write-uint16 (value str)
  (nibbles:write-ub16/be value str))

(defun read-int16 (str)
  (nibbles:read-sb16/be str))

(defun write-int16 (value str)
  (nibbles:write-sb16/be value str))

(defun read-uint32 (str)
  (nibbles:read-ub32/be str))

(defun write-uint32 (value str)
  (nibbles:write-ub32/be value str))

(defun read-int32 (str)
  (nibbles:read-sb32/be str))

(defun write-int32 (value str)
  (nibbles:write-sb32/be value str))

(defun read-float (str)
  (nibbles:read-ieee-single/be str))

(defun write-float (value str)
  (nibbles:write-ieee-single/be value str))

(defun read-double (str)
  (nibbles:read-ieee-double/be str))

(defun write-double (value str)
  (nibbles:write-ieee-double/be value str))

;;;; IDX FOMAT -----------------------------------------------------------------
(defun %read-idx (str)
  (if (= (read-uint16 str) 0)
      (let* ((dtype (read-uint8 str))
             (rank (read-uint8 str))
             (shape (loop repeat rank collect (read-uint32 str)))
             (num-elems (reduce #'* shape))
             (elm-type (element-type dtype))
             (read-fn (get-read-fn dtype))
             (data (make-array shape :element-type elm-type))
             (data1d (make-array num-elems :element-type elm-type :displaced-to data)))
        (loop for i to (- num-elems 1) do
             (setf (aref data1d i)
                   (funcall read-fn str)))
        data)
        '()))

(defun %write-idx (str data)
  (let* ((dtype (get-dtype data))
         (shape (array-dimensions data))
         (rank (length shape))
         (num-elems (reduce #'* shape))
         (elm-type (array-element-type data))
         (write-fn (get-write-fn dtype))
         (data1d (make-array num-elems :element-type elm-type :displaced-to data)))
    ;; magic
    (write-uint16 0 str)
    ;; dtype
    (write-uint8 dtype str)
    ;; rank
    (write-uint8 rank str)
    ;; dimensions
    (loop for dim in shape do
         (write-uint32 dim str))
    ;; data
    (loop for i to (- num-elems 1) do
         (funcall write-fn (aref data1d i) str))))

;;;; API -----------------------------------------------------------------------
(defun read-from-stream (str)
  "Read IDX stream"
  (%read-idx str))

(defun read-from-file (filename)
  "Read IDX file"
  (with-open-file (str filename :direction :input :element-type '(unsigned-byte 8))
    (read-from-stream str)))

(defun write-to-stream (str data)
  "Write an array `data` to a stream `str` in the IDX file format."
  (%write-idx str data)
  (finish-output str))

(defun write-to-file (filename data &key (if-exists nil if-exists-given))
  "Write an array `data` to a file at `filename` in the IDX file format."
  (if if-exists-given
      (with-open-file (str filename :direction :output :if-exists if-exists :element-type '(unsigned-byte 8))
        (write-to-stream str data))
      (with-open-file (str filename :direction :output :element-type '(unsigned-byte 8))
        (write-to-stream str data))))
