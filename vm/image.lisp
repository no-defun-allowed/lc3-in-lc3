(in-package :lc3)

(defun make-image ()
  (make-array 256 :initial-element nil))
(defun make-page ()
  (make-array 256 :initial-element 0))

(defun write-word-to-image (image address word)
  (multiple-value-bind (page-number offset)
      (floor address 256)
    (let ((page (aref image page-number)))
      (when (null page)
        (setf page (make-page)
              (aref image page-number) page))
      (setf (aref page offset) word))))

(defun emit-image (image initial-pc)
  (comment "Virtual memory" :big t)
  (label initial-program-counter)
  (literal initial-pc)
  (label next-page)
  (literal 'end-of-file)
  ;; Emit page table
  (label page-table)
  (loop for page across image
        for number from 0
        do (literal (if (null page) 0 (format nil "page-~d" number))))
  ;; Emit page data
  (loop for page across image
        for number from 0
        unless (null page)
          do (format t "~&page_~d" number)
             (loop for word across page
                   do (literal word)))
  ;; Emit next page pointer
  (comment "This should be at the bottom of the file")
  (label end-of-file)
  (literal 0))

(defun read-image (pathname)
  (with-open-file (s pathname
                     :element-type '(unsigned-byte 8)
                     :if-does-not-exist :error)
    (let* ((initial-pc (nibbles:read-ub16/be s))
           (pc initial-pc)
           (image (make-image)))
      (handler-case
          (loop
            (progn
              (write-word-to-image image
                                   pc
                                   (nibbles:read-ub16/be s))
              (incf pc)))
        (end-of-file ()))
      (emit-image image initial-pc))))
